{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell      #-}
{- |
   Module     : Shakebook.Conventions
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Conventions used in Shakebook projects, common lenses, generators, and indexing wrappers over Values.
-}
module Shakebook.Conventions where

import           Composite.Aeson
import           Composite.Record
import           Composite.TH
import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Data.Binary.Instances.Time ()
import           Data.Hashable.Time
import           Data.IxSet.Typed           as Ix
import           Development.Shake.Plus     hiding ((:->))
import           RIO
import qualified RIO.Text                   as T
import           RIO.Time
import           Shakebook.Aeson
import qualified Shakebook.Feed             as Atom
import Shakebook.Lucid
import           Shakebook.Sitemap
import Composite.Aeson.Path
import Data.TypeRepMap
import GHC.Exts
import Data.Typeable
import Data.Vinyl hiding (RElem)

withLensesAndProxies [d|
  type FId            = "id"           :-> Text
  type FUrl           = "url"          :-> Text
  |]

type Link = '[FId, FUrl]

withLensesAndProxies [d|
  type FCdnImports    = "cdn-imports"  :-> HtmlFragment
  type FContent       = "content"      :-> Text
  type FDescription   = "description"  :-> Text
  type FHighlighting  = "highlighting" :-> StyleFragment
  type FImage         = "image"        :-> Maybe Text

  type FModified      = "modified"     :-> UTCTime
  type FNext          = "next"         :-> HtmlFragment
  type FPageNo        = "pageno"       :-> Int
  type FPageLinks     = "page-links"   :-> HtmlFragment
  type FPrettyDate    = "pretty-date"  :-> UTCTime
  type FPrevious      = "previous"     :-> HtmlFragment
  type FPosted        = "posted"       :-> UTCTime
  type FItems x       = "items"        :-> [Record x]
  type FRecentPosts x = "recent-posts" :-> [Record x]
  type FSiteTitle     = "site-title"   :-> Text
  type FSrcPath       = "src-path"     :-> Path Rel File
  type FSocial        = "social"       :-> [Record Link]
  type FSubsections x = "subsections"  :-> [Record x]
  type FTags          = "tags"         :-> [Text]
  type FTagLinks      = "tag-links"    :-> [Record Link]
  type FTeaser        = "teaser"       :-> Text
  type FTitle         = "title"        :-> Text
  type FToc           = "toc"          :-> HtmlFragment

  |]

-- | Tag indices for a `Post` for use with `IxSet`.
newtype Tag = Tag { unTag :: Text }
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

-- | Posted index for a `Post` for use with `IxSet`.
newtype Posted = Posted { unPosted :: UTCTime }
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

-- | YearMonth (yyyy, mm) index for a `Post` for use with `IxSet`.
newtype YearMonth = YearMonth { unYearMonth :: (Integer, Int) }
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

toYearMonth :: UTCTime -> YearMonth
toYearMonth = (\(a, b, _) -> YearMonth (a, b)) . toGregorian . utctDay

fromYearMonth :: YearMonth -> UTCTime
fromYearMonth (YearMonth (y,m)) = UTCTime (fromGregorian y m 1) 0

instance Ix.Indexable '[Tag, Posted, YearMonth] (Record Stage1Post) where
  indices = Ix.ixList (Ix.ixFun (fmap Tag . view fTags))
                      (Ix.ixFun (pure . Posted . view fPosted))
                      (Ix.ixFun (pure . toYearMonth . view fPosted))

asSitemapUrl :: (RElem FUrl xs, RElem FPosted xs) => Text -> Record xs -> SitemapUrl
asSitemapUrl baseUrl x = SitemapUrl {
   sitemapLocation = baseUrl <> view fUrl x
 , sitemapLastModified = Just (view fPosted x)
 , sitemapChangeFrequency = Nothing
 , sitemapPriority = Nothing
}

-- | Convert a Post to an Atom Entry
asAtomEntry :: (RElem FContent xs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs) => Record xs -> Atom.Entry
asAtomEntry x = (Atom.nullEntry (view fUrl x)
                  (Atom.TextString $ view fTitle x)
                  (T.pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ view fPosted x)) {
                    Atom.entryContent = Just $ Atom.TextContent (view fContent x)
                  }

fModifiedField :: JsonField e FModified
fModifiedField = field iso8601DateTimeJsonFormat

fPrettyDateField :: JsonField e FPrettyDate
fPrettyDateField = field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%A, %B %d, %Y" "yyyy-mm-dd" :| []))

fIdField :: JsonField e FId
fIdField = field textJsonFormat

fUrlField :: JsonField e FUrl
fUrlField = field textJsonFormat

defaultFields :: TypeRepMap (JsonField e)
defaultFields = GHC.Exts.fromList [ WrapTypeable fModifiedField
                                  , WrapTypeable fPrettyDateField
                                  , WrapTypeable fIdField
                                  , WrapTypeable fUrlField]

a :: (Typeable x, Show x, MonadThrow m) => TypeRepMap (JsonField e) -> Identity x -> m ((JsonField e) x)
a fs (Identity x) = case Data.TypeRepMap.lookup fs of
                      Just x -> return x
                      Nothing -> throwM $ F $ "Could not find " <> show (typeOf x)

rtraverse'
  :: (AllHave '[Typeable, Show] rs, Applicative h)
  => (forall x. (Typeable x, Show x) => f x -> h (g x))
  -> Rec f rs
  -> h (Rec g rs)
rtraverse' _ RNil      = pure RNil
rtraverse' f (x :& xs) = (:&) <$> f x <*> rtraverse' f xs

b :: (AllHave '[Typeable, Show] x, MonadThrow m) => Rec Identity x -> m (Rec (JsonField e) x)
b = rtraverse' (a defaultFields)

newtype F = F String deriving (Show, Exception)
--- Stage 0 Types

-- "Basic Markdown" - These two fields are always populated by the markdown loader - the source path and the main body content.
type BasicMD = FSrcPath : FContent : '[]

basicMDJsonFormatRecord :: JsonFormatRecord e BasicMD
basicMDJsonFormatRecord = defaultJsonFormatRecord

-- A 'RawDoc' contains three mandatory fields - title, description and modified.
type RawDoc = FModified : FTitle : FDescription : BasicMD

rawDocJsonFormatRecord :: JsonFormatRecord e RawDoc
rawDocJsonFormatRecord = field iso8601DateTimeJsonFormat
                      :& defaultJsonFormatRecord

rawDocJsonFormat :: JsonFormat e (Record RawDoc)
rawDocJsonFormat = recordJsonFormat rawDocJsonFormatRecord

-- A `RawPost` contains three mandatory fields, title, tags, and posted, and an optional image field.
type RawPost = FPosted : FImage : FTitle : FTags : BasicMD

rawPostJsonFormatRecord :: JsonFormatRecord e RawPost
rawPostJsonFormatRecord = field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%F" "yyyy-mm-dd" :| []))
                       :& optionalField textJsonFormat
                       :& defaultJsonFormatRecord

rawPostJsonFormat :: JsonFormat e (Record RawPost)
rawPostJsonFormat = recordJsonFormat rawPostJsonFormatRecord

-- A `RawSingle` is a post without any tags or date information.
type RawSingle = FImage : FTitle : BasicMD

rawSingleJsonFormatRecord :: JsonFormatRecord e RawSingle
rawSingleJsonFormatRecord = optionalField textJsonFormat
                         :& defaultJsonFormatRecord

rawSingleJsonFormat :: JsonFormat e (Record RawSingle)
rawSingleJsonFormat = recordJsonFormat rawSingleJsonFormatRecord

--- Stage 1 Types

-- Simple link object, used in a list for tags and social links.

linkJsonFormatRecord :: JsonFormatRecord e Link
linkJsonFormatRecord = field textJsonFormat :& field textJsonFormat :& RNil

linkJsonFormat :: JsonFormat e (Record Link)
linkJsonFormat = recordJsonFormat linkJsonFormatRecord

type URLised x = FUrl : x

urlisedXJsonFormatRecord :: JsonFormatRecord e x -> JsonFormatRecord e (URLised x)
urlisedXJsonFormatRecord x = field textJsonFormat :& x

type Stage1Post = FPrettyDate : FTagLinks : FTeaser : URLised RawPost

stage1PostJsonFormatRecord :: JsonFormatRecord e Stage1Post
stage1PostJsonFormatRecord = field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%A, %B %d, %Y" "yyyy-mm-dd" :| []))
                          :& field (listJsonFormat linkJsonFormat)
                          :& field textJsonFormat
                          :& urlisedXJsonFormatRecord rawPostJsonFormatRecord

stage1PostJsonFormat :: JsonFormat e (Record Stage1Post)
stage1PostJsonFormat = recordJsonFormat stage1PostJsonFormatRecord

type Stage1Doc = URLised RawDoc

stage1DocJsonFormatRecord :: JsonFormatRecord e Stage1Doc
stage1DocJsonFormatRecord = urlisedXJsonFormatRecord rawDocJsonFormatRecord

stage1DocJsonFormat :: JsonFormat e (Record Stage1Doc)
stage1DocJsonFormat = recordJsonFormat stage1DocJsonFormatRecord

--- Stage 2 Types

-- Enrichment provides fields most pages display or can otherwise be safely ignored such as highlighting, json imports and social links.
type FinalDoc = FToc : FSubsections Stage1Doc : Stage1Doc

finalDocJsonFormatRecord :: JsonFormatRecord e FinalDoc
finalDocJsonFormatRecord = field htmlJsonFormat
                        :& field (listJsonFormat stage1DocJsonFormat)
                        :& stage1DocJsonFormatRecord

type FinalPost = FToc : Stage1Post

finalPostJsonFormatRecord :: JsonFormatRecord e FinalPost
finalPostJsonFormatRecord = field htmlJsonFormat
                         :& stage1PostJsonFormatRecord

type IndexPage x = FPageLinks : FToc : FTitle : FUrl : FItems x : FPageNo : '[]

indexPageJsonFormatRecord :: JsonFormat e (Record x) -> JsonFormatRecord e (IndexPage x)
indexPageJsonFormatRecord x = field htmlJsonFormat
                           :& field htmlJsonFormat
                           :& field textJsonFormat
                           :& field textJsonFormat
                           :& field (listJsonFormat x)
                           :& field integralJsonFormat
                           :& RNil

type PostIndexPage = IndexPage Stage1Post

postIndexPageJsonFormatRecord :: JsonFormatRecord e PostIndexPage
postIndexPageJsonFormatRecord = indexPageJsonFormatRecord stage1PostJsonFormat

postIndexPageJsonFormat :: JsonFormat e (Record PostIndexPage)
postIndexPageJsonFormat = recordJsonFormat $ indexPageJsonFormatRecord stage1PostJsonFormat

type MainPage = FRecentPosts Stage1Post : RawSingle

mainPageJsonFormatRecord :: JsonFormatRecord e MainPage
mainPageJsonFormatRecord = field (listJsonFormat stage1PostJsonFormat)
                        :& rawSingleJsonFormatRecord

mainPageJsonFormat :: JsonFormat e (Record MainPage)
mainPageJsonFormat = recordJsonFormat mainPageJsonFormatRecord
