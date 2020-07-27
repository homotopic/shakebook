{-# LANGUAGE UndecidableInstances #-}
{- |
   Module     : Shakebook.Conventions
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Conventions used in Shakebook projects, common lenses, generators, and indexing wrappers over Values.
-}
module Shakebook.Conventions (
  -- * Fields
  FCdnImports
, FContent
, FDescription
, FHighlighting
, FItems
, FPageLinks
, FPosted
, FImage
, FModified
, FNext
, FRecentPosts
, FPrettyDate
, FPrevious
, FSiteTitle
, FSocial
, FSrcPath
, FSubsections
, FTags
, FTagLinks
, FTeaser
, FTitle
, FToc
, FUrl
, FPageNo

  -- * Lenses
, viewContent
, viewImage
, viewModified
, viewPageNo
, viewPosted
, viewSrcPath
, viewTags
, viewTitle
, viewUrl

  -- * Generations
, genBlogNav
, genDocNav
, addDerivedUrl
, asSitemapUrl
, asAtomEntry

  -- * Indexing
, Link
, Tag(..)
, Posted(..)
, YearMonth(..)
, fromYearMonthPair
, toYearMonthPair

  -- * Stages
, RawPost
, Stage1Post
, RawDoc
, Stage1Doc
, PostSet

  -- * Formatting
, basicMDJsonFormatRecord
, basicMDJsonFormat
, rawDocJsonFormat
, rawPostJsonFormat
, rawSingleJsonFormat
, stage1PostJsonFormat
, stage1DocJsonFormat
, finalPostJsonFormat
, mainPageJsonFormat
, indexPageJsonFormatRecord
, postIndexPageJsonFormat
, finalDocJsonFormat

  -- * Templates
, TMain
, TDoc
, TPost
, TPostIndex
, Enriched
) where

import           Composite.Aeson
import Data.Binary.Instances.Time()
import           Composite.Record
import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Data.Hashable.Time
import           Data.IxSet.Typed         as Ix
import           Development.Shake.Plus   hiding ((:->))
import           Lucid
import           RIO
import           RIO.List
import qualified RIO.Text                 as T
import           RIO.Time
import           Shakebook.Aeson
import qualified Shakebook.Feed           as Atom
import           Shakebook.Lucid()
import           Shakebook.Sitemap
import           Text.Pandoc.Highlighting
import Data.Binary
import Path.Binary

type FCdnImports    = "cdn-imports"  :-> Html ()
type FContent       = "content"      :-> Text
type FDescription   = "description"  :-> Text
type FHighlighting  = "highlighting" :-> Style
type FImage         = "image"        :-> Maybe Text
type FId            = "id"           :-> Text
type FModified      = "modified"     :-> UTCTime
type FNext          = "next"         :-> Html ()
type FPageNo        = "pageno"       :-> Int
type FPageLinks     = "page-links"   :-> [Record Link]
type FPrettyDate    = "pretty-date"  :-> UTCTime
type FPrevious      = "previous"     :-> Html ()
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
type FToc           = "toc"          :-> Html ()
type FUrl           = "url"          :-> Text


instance Binary (Record '[])

instance (Binary a, Binary (Record xs), x ~ (s :-> a)) => Binary (Record (x : xs)) where
  put (x :*: xs) = put x >> put xs
  get = liftA2 (:*:) get get

--instance Binary (Record Link)

--instance Binary (Record Stage1Post)

instance NFData (Record xs) where
  rnf x = seq x ()

instance Hashable (Record '[]) where
  hashWithSalt n RNil = n `hashWithSalt` ()

instance (Hashable a, Hashable (Record xs), x ~ (s :-> a)) => Hashable (Record (x : xs)) where
  hashWithSalt n (x :*: xs) = n `hashWithSalt` x `hashWithSalt` xs

instance Hashable a => Hashable (s :-> a) where
  hashWithSalt n x = hashWithSalt n $ getVal x

instance Binary a => Binary (s :-> a) where
  put = put . getVal
  get = fmap (runIdentity . val) get

instance NFData (s :-> a) where
  rnf x = seq x ()


-- | View the "image" field of a JSON value.
viewContent :: RElem FContent xs => Record xs -> Text
viewContent = view (rlens (Proxy @FContent))

-- | View the "image" field of a JSON value.
viewImage :: RElem FImage xs => Record xs -> Maybe Text
viewImage = view (rlens (Proxy @FImage))

-- | View the "modified" field of a JSON value.
viewModified :: RElem FModified xs => Record xs -> UTCTime
viewModified = view (rlens (Proxy @FModified))

-- | View the "pageno" field of a JSON Value as a UTCTime.
viewPageNo :: RElem FPageNo xs => Record xs -> Int
viewPageNo = view (rlens (Proxy @FPageNo))

-- | View the "date" field of a JSON Value as a UTCTime.
viewPosted :: RElem FPosted xs => Record xs -> UTCTime
viewPosted = view (rlens (Proxy @FPosted))

-- | View the "tags" field of a JSON Value as a list.
viewSrcPath :: RElem FSrcPath xs => Record xs -> Path Rel File
viewSrcPath = view (rlens (Proxy @FSrcPath))

-- | View the "tags" field of a JSON Value as a list.
viewTags :: RElem FTags xs => Record xs -> [Text]
viewTags = view (rlens (Proxy @FTags))

-- | View the "title" field of a JSON Value.
viewTitle :: RElem FTitle xs => Record xs -> Text
viewTitle = view (rlens (Proxy @FTitle))

-- | View the "url" field of a Record.
viewUrl :: RElem FUrl xs => Record xs -> Text
viewUrl = view (rlens (Proxy @FUrl))

-- | Tag indices for a `Post` for use with `IxSet`.
newtype Tag = Tag Text
  deriving (Show, Eq, Ord, Data, Typeable, Hashable, Binary, NFData, Generic)

-- | Posted index for a `Post` for use with `IxSet`.
newtype Posted = Posted UTCTime
  deriving (Show, Eq, Ord, Data, Typeable, Hashable, Generic, NFData)

-- | YearMonth (yyyy, mm) index for a `Post` for use with `IxSet`.
newtype YearMonth = YearMonth (Integer, Int)
  deriving (Show, Eq, Ord, Data, Typeable, Hashable, Binary, NFData, Generic)

toYearMonthPair :: UTCTime -> (Integer, Int)
toYearMonthPair = (\(a, b, _) -> (a, b)) . toGregorian . utctDay

fromYearMonthPair :: (Integer, Int) -> UTCTime
fromYearMonthPair (y,m) = UTCTime (fromGregorian y m 1) 0

-- | Create a blog navbar object for a posts section, with layers "toc1", "toc2", and "toc3".
genBlogNav :: (IsIndexOf YearMonth ixs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs)
           => Text -- ^ "Top level title, e.g "Blog"
           -> Text -- ^ Root page, e.g "/posts"
           -> (UTCTime -> Text) -- ^ Formatting function to a UTCTime to a title.
           -> (UTCTime -> Text) -- ^ Formatting function to convert a UTCTime to a URL link
           -> IxSet ixs (Record xs)
           -> Html ()
genBlogNav a b f g xs =
  ul_ $
    li_ $ do
      a_ [href_ b] (toHtml a)
      ul_ $ forM_ (groupDescBy xs) $ \(YearMonth (y, m), xs') -> do
        let t' = fromYearMonthPair (y, m)
        li_ $ a_ [href_ $ g t'] (toHtml $ f t')
        ul_ $ forM (sortOn (Down . viewPosted) xs') $ \x ->
          li_ $ a_ [href_ $ viewUrl x] (toHtml $ viewTitle x)

-- | Create a toc navbar object for a docs section, with layers "toc1", "toc2" and "toc3".
genDocNav :: (RElem FUrl xs, RElem FTitle xs) => Cofree [] (Record xs) -> Html ()
genDocNav (x :< xs) = ul_ $ li_ $ do
      a_ [href_ $ viewUrl x] (toHtml $ viewTitle x)
      forM_ xs genDocNav

asSitemapUrl :: (RElem FUrl xs, RElem FPosted xs) => Text -> Record xs -> SitemapUrl
asSitemapUrl baseUrl x = SitemapUrl {
   sitemapLocation = baseUrl <> viewUrl x
 , sitemapLastModified = Just (viewPosted x)
 , sitemapChangeFrequency = Nothing
 , sitemapPriority = Nothing
}

-- | Convert a Post to an Atom Entry
asAtomEntry :: (RElem FContent xs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs) => Record xs -> Atom.Entry
asAtomEntry x = (Atom.nullEntry (viewUrl x)
                  (Atom.TextString $ viewTitle x)
                  (T.pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ viewPosted x)) {
                    Atom.entryContent = Just $ Atom.TextContent (viewContent x)
                  }

addDerivedUrl :: (MonadThrow m, RElem FSrcPath xs) => (Path Rel File -> m Text) -> Record xs -> m (Record (FUrl : xs))
addDerivedUrl f xs = f (viewSrcPath xs) >>= \x -> return $ x :*: xs

--- Stage 0 Types

-- "Basic Markdown" - These two fields are always populated by the markdown loader - the source path and the main body content.
type BasicMD = FSrcPath : FContent : '[]

basicMDJsonFormatRecord :: JsonFormatRecord e BasicMD
basicMDJsonFormatRecord = field relFileJsonFormat
                       :& field textJsonFormat
                       :& RNil

basicMDJsonFormat :: JsonFormat e (Record BasicMD)
basicMDJsonFormat = recordJsonFormat basicMDJsonFormatRecord

-- A 'RawDoc' contains three mandatory fields - title, description and modified.
type RawDoc = FDescription : FTitle : FModified : BasicMD

rawDocJsonFormatRecord :: JsonFormatRecord e RawDoc
rawDocJsonFormatRecord = field aesonJsonFormat
                      :& field defaultJsonFormat
                      :& field iso8601DateTimeJsonFormat
                      :& basicMDJsonFormatRecord

rawDocJsonFormat :: JsonFormat e (Record RawDoc)
rawDocJsonFormat = recordJsonFormat rawDocJsonFormatRecord

-- A `RawPost` contains three mandatory fields, title, tags, and posted, and an optional image field.
type RawPost = FTitle : FImage : FTags : FPosted : BasicMD

rawPostJsonFormatRecord :: JsonFormatRecord e RawPost
rawPostJsonFormatRecord = field textJsonFormat
                       :& optionalField textJsonFormat
                       :& field (listJsonFormat textJsonFormat)
                       :& field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%F" "yyyy-mm-dd" :| []))
                       :& basicMDJsonFormatRecord

rawPostJsonFormat :: JsonFormat e (Record RawPost)
rawPostJsonFormat = recordJsonFormat rawPostJsonFormatRecord

-- A `RawSingle` is a post without any tags or date information.
type RawSingle = FTitle : FImage : BasicMD

rawSingleJsonFormatRecord :: JsonFormatRecord e RawSingle
rawSingleJsonFormatRecord = field textJsonFormat
                         :& optionalField textJsonFormat
                         :& basicMDJsonFormatRecord

rawSingleJsonFormat :: JsonFormat e (Record RawSingle)
rawSingleJsonFormat = recordJsonFormat rawSingleJsonFormatRecord

--- Stage 1 Types

-- Simple link object, used in a list for tags and social links.
type Link = '[FId, FUrl]

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
type Enriched x = FSocial : FCdnImports : FHighlighting : FSiteTitle : x

enrichedXJsonFormatRecord :: JsonFormatRecord e x -> JsonFormatRecord e (Enriched x)
enrichedXJsonFormatRecord x = field (listJsonFormat linkJsonFormat)
                           :& field lucidJsonFormat
                           :& field styleJsonFormat
                           :& field defaultJsonFormat
                           :& x

type FinalDoc = FToc : FSubsections Stage1Doc : Enriched Stage1Doc

finalDocJsonFormatRecord :: JsonFormatRecord e FinalDoc
finalDocJsonFormatRecord = field lucidJsonFormat
                        :& field (listJsonFormat stage1DocJsonFormat)
                        :& enrichedXJsonFormatRecord stage1DocJsonFormatRecord

finalDocJsonFormat :: JsonFormat e (Record FinalDoc)
finalDocJsonFormat = recordJsonFormat finalDocJsonFormatRecord

type FinalPost = FToc : Enriched Stage1Post

finalPostJsonFormatRecord :: JsonFormatRecord e FinalPost
finalPostJsonFormatRecord = field lucidJsonFormat
                         :& enrichedXJsonFormatRecord stage1PostJsonFormatRecord

finalPostJsonFormat :: JsonFormat e (Record FinalPost)
finalPostJsonFormat = recordJsonFormat finalPostJsonFormatRecord

type IndexPage x = Enriched (FPageLinks : FToc : FTitle : FUrl : FItems x : FPageNo : '[])

indexPageJsonFormatRecord :: JsonFormat e (Record x) -> JsonFormatRecord e (IndexPage x)
indexPageJsonFormatRecord x = enrichedXJsonFormatRecord $ field (listJsonFormat linkJsonFormat)
                                                       :& field lucidJsonFormat
                                                       :& field textJsonFormat
                                                       :& field textJsonFormat
                                                       :& field (listJsonFormat x)
                                                       :& field integralJsonFormat
                                                       :& RNil

type PostIndexPage = IndexPage Stage1Post

postIndexPageJsonFormat :: JsonFormat e (Record PostIndexPage)
postIndexPageJsonFormat = recordJsonFormat $ indexPageJsonFormatRecord stage1PostJsonFormat

type MainPage = FRecentPosts Stage1Post : Enriched RawSingle

mainPageJsonFormatRecord :: JsonFormatRecord e MainPage
mainPageJsonFormatRecord = field (listJsonFormat stage1PostJsonFormat)
                        :& enrichedXJsonFormatRecord rawSingleJsonFormatRecord

mainPageJsonFormat :: JsonFormat e (Record MainPage)
mainPageJsonFormat = recordJsonFormat mainPageJsonFormatRecord

instance Ix.Indexable '[Tag, Posted, YearMonth] (Record Stage1Post) where
  indices = Ix.ixList (Ix.ixFun (fmap Tag . viewTags))
                      (Ix.ixFun (pure . Posted . viewPosted))
                      (Ix.ixFun (pure . YearMonth . toYearMonthPair . viewPosted))

type PostSet x = Ix.IxSet '[Tag, Posted, YearMonth] (Record x)

type TMain      = "templates/index.html" :-> Record MainPage
type TDoc       = "templates/docs.html"  :-> Record FinalDoc
type TPost      = "templates/post.html"  :-> Record FinalPost
type TPostIndex = "templates/post-list.html" :-> Record PostIndexPage
