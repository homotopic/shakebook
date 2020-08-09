{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell      #-}
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


, fCdnImports
, fContent
, fDescription
, fHighlighting
, fItems
, fPageLinks
, fPosted
, fImage
, fModified
, fNext
, fRecentPosts
, fPrettyDate
, fPrevious
, fSiteTitle
, fSocial
, fSrcPath
, fSubsections
, fTags
, fTagLinks
, fTeaser
, fTitle
, fToc
, fUrl
, fPageNo
, fId

  -- * Generations
, genBlogNav
, genDocNav
, addDerivedUrl
, asSitemapUrl
, asAtomEntry
, addTagLinks
, addPrettyDate
, addTeaser

  -- * Indexing
, Link
, Tag(..)
, Posted(..)
, YearMonth(..)
, fromYearMonth
, toYearMonth

  -- * Stages
, RawPost
, Stage1Post
, RawDoc
, Stage1Doc
, Enrichment

  -- * Oracles
, BlogNav(..)
, DocNav(..)
, IndexRoot(..)
, IndexPages(..)
, RecentPosts(..)
, PostsFilter(..)
, indexFilter
, defaultIndexRoots
, defaultIndexPages

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
, MainPage
, FinalPost
, FinalDoc
, PostIndexPage

, IndexHtml(..)
, DocSubsectionMap(..)
, DocSubsections(..)
, DocHtml(..)
, PostHtml(..)
, PostIndexHtml(..)

  -- * Templates
, Enriched
) where

import           Composite.Aeson
import           Composite.Record
import           Composite.TH
import Data.Vinyl hiding (RElem)
import Data.Vinyl.TypeLevel
import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Data.Binary.Instances.Time ()
import           Data.Hashable.Time
import           Data.IxSet.Typed           as Ix
import           Development.Shake.Plus     hiding ((:->))
import           Development.Shake.Plus.Extended
import           Lucid
import Lucid.Base
import           RIO
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Text                   as T
import qualified RIO.Text.Partial           as T
import           RIO.Time
import           Shakebook.Aeson
import           Shakebook.Defaults
import qualified Shakebook.Feed             as Atom
import Shakebook.Lucid
import           Shakebook.Sitemap
import           Text.Pandoc.Highlighting
import Control.Comonad.Zipper.Extra

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
  type FPageLinks     = "page-links"   :-> [Record Link]
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
newtype Tag = Tag Text
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

-- | Posted index for a `Post` for use with `IxSet`.
newtype Posted = Posted UTCTime
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

-- | YearMonth (yyyy, mm) index for a `Post` for use with `IxSet`.
newtype YearMonth = YearMonth (Integer, Int)
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

toYearMonth :: UTCTime -> YearMonth
toYearMonth = (\(a, b, _) -> YearMonth (a, b)) . toGregorian . utctDay

fromYearMonth :: YearMonth -> UTCTime
fromYearMonth (YearMonth (y,m)) = UTCTime (fromGregorian y m 1) 0

-- | Create a blog navbar object for a posts section, with layers "toc1", "toc2", and "toc3".
genBlogNav :: (IsIndexOf YearMonth ixs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs, MonadAction m)
           => Text -- ^ "Top level title, e.g "Blog"
           -> (UTCTime -> Text) -- ^ Formatting function to a UTCTime to a title.
           -> IxSet ixs (Record xs)
           -> m (Html ())
genBlogNav a f xs = commuteHtmlT $ do
  ul_ $
    li_ $ do
      b <- lift $ askOracle $ IndexRoot AllPosts
      a_ [href_ b] (toHtml a)
      ul_ $ forM_ (groupDescBy xs) $ \(ym, xs') -> do
        k <- lift $ askOracle $ IndexRoot $ ByYearMonth ym
        li_ $ a_ [href_ k] (toHtml . f $ fromYearMonth ym)
        ul_ $ forM (sortOn (Down . view fPosted) xs') $ \x ->
          li_ $ a_ [href_ $ view fUrl x] (toHtml $ view fTitle x)

-- | Create a toc navbar object for a docs section, with layers "toc1", "toc2" and "toc3".
genDocNav :: (RElem FUrl xs, RElem FTitle xs) => Cofree [] (Record xs) -> Html ()
genDocNav (x :< xs) = ul_ $ li_ $ do
      a_ [href_ $ view fUrl x] (toHtml $ view fTitle x)
      forM_ xs genDocNav

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

addDerivedUrl :: (MonadThrow m, RElem FSrcPath xs) => (Path Rel File -> m Text) -> Record xs -> m (Record (FUrl : xs))
addDerivedUrl f xs = f (view fSrcPath xs) >>= \x -> return $ x :*: xs

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
type Enrichment = FSocial : FCdnImports : FHighlighting : FSiteTitle : '[]

type Enriched x = Enrichment ++ x

enrichedXJsonFormatRecord :: JsonFormatRecord e x -> JsonFormatRecord e (Enriched x)
enrichedXJsonFormatRecord x = field (listJsonFormat linkJsonFormat)
                           :& field htmlJsonFormat
                           :& field styleJsonFormat
                           :& field defaultJsonFormat
                           :& x

type FinalDoc = FToc : FSubsections Stage1Doc : Enriched Stage1Doc

finalDocJsonFormatRecord :: JsonFormatRecord e FinalDoc
finalDocJsonFormatRecord = field htmlJsonFormat
                        :& field (listJsonFormat stage1DocJsonFormat)
                        :& enrichedXJsonFormatRecord stage1DocJsonFormatRecord

finalDocJsonFormat :: JsonFormat e (Record FinalDoc)
finalDocJsonFormat = recordJsonFormat finalDocJsonFormatRecord

type FinalPost = FToc : Enriched Stage1Post

finalPostJsonFormatRecord :: JsonFormatRecord e FinalPost
finalPostJsonFormatRecord = field htmlJsonFormat
                         :& enrichedXJsonFormatRecord stage1PostJsonFormatRecord

finalPostJsonFormat :: JsonFormat e (Record FinalPost)
finalPostJsonFormat = recordJsonFormat finalPostJsonFormatRecord

type IndexPage x = Enriched (FPageLinks : FToc : FTitle : FUrl : FItems x : FPageNo : '[])

indexPageJsonFormatRecord :: JsonFormat e (Record x) -> JsonFormatRecord e (IndexPage x)
indexPageJsonFormatRecord x = enrichedXJsonFormatRecord $ field (listJsonFormat linkJsonFormat)
                                                       :& field htmlJsonFormat
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

addTagLinks :: (MonadAction m, RElem FTags xs) => Record xs -> m (Record (FTagLinks : xs))
addTagLinks xs = do
  ks <- forM (view fTags xs) $ \x -> do
          u <- askOracle . IndexRoot . ByTag . Tag $ x
          return (x :*: u :*: RNil)
  return $ ks :*: xs

addTeaser :: RElem FContent xs => Record xs -> Record (FTeaser : xs)
addTeaser xs = head (T.splitOn "<!-- more -->" (view fContent xs)) :*: xs

addPrettyDate :: RElem FPosted xs => Record xs -> Record (FPrettyDate : xs)
addPrettyDate xs = view fPosted xs :*: xs


instance Ix.Indexable '[Tag, Posted, YearMonth] (Record Stage1Post) where
  indices = Ix.ixList (Ix.ixFun (fmap Tag . view fTags))
                      (Ix.ixFun (pure . Posted . view fPosted))
                      (Ix.ixFun (pure . toYearMonth . view fPosted))

newtype BlogNav a = BlogNav a
  deriving newtype (Eq, Show, Generic, NFData, Hashable, Binary)

type instance RuleResult (BlogNav a) = HtmlFragment

newtype DocNav a = DocNav a
  deriving newtype (Eq, Show, Generic, Binary, Hashable, NFData)

type instance RuleResult (DocNav a) = HtmlFragment

newtype RecentPosts a = RecentPosts a
  deriving newtype (Eq, Show, Generic, Binary, Hashable, NFData)

type instance RuleResult (RecentPosts ()) = [Record Stage1Post]

data PostsFilter = AllPosts | ByTag Tag | ByYearMonth YearMonth
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable, Binary)

newtype IndexRoot a = IndexRoot a
  deriving newtype (Eq, Show, Generic, Binary, Hashable, NFData)

type instance RuleResult (IndexRoot a) = Text

newtype IndexPages a = IndexPages a
  deriving newtype (Eq, Show, Generic, Binary, Hashable, NFData)

type instance RuleResult (IndexPages a) = [Record (FUrl : FItems Stage1Post : FPageNo : '[])]

newtype IndexHtml = IndexHtml (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

newtype PostHtml  = PostHtml (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

newtype DocHtml  = DocHtml (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

data PostIndexHtml = PostIndexHtml Text PostsFilter Int
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NFData, Binary, Hashable)

type instance RuleResult IndexHtml = Record MainPage
type instance RuleResult PostHtml  = Record FinalPost
type instance RuleResult DocHtml = Record FinalDoc
type instance RuleResult PostIndexHtml = Record PostIndexPage

type instance RuleResult DocSubsections = [Record Stage1Doc]

newtype DocSubsections = DocSubsections (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

newtype DocSubsectionMap a = DocSubsectionMap a
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

type instance RuleResult (DocSubsectionMap a) = HashMap (Path Rel File) [Path Rel File]

indexFilter :: (Indexable ixs a, IsIndexOf Tag ixs,
                    IsIndexOf YearMonth ixs) =>
                    PostsFilter -> IxSet ixs a -> IxSet ixs a
indexFilter x = case x of
                  AllPosts      -> id
                  ByTag t       -> (Ix.@+ [t])
                  ByYearMonth t -> (Ix.@+ [t])

defaultIndexRoots :: MonadAction m => IndexRoot PostsFilter -> m Text
defaultIndexRoots (IndexRoot x) = case x of
     AllPosts       -> return "/posts/"
     ByTag (Tag t)  -> (<> "tags/" <> t <> "/") <$> askOracle (IndexRoot AllPosts)
     ByYearMonth ym -> (<> "months/" <> defaultMonthUrlFormat (fromYearMonth ym) <> "/") <$> askOracle (IndexRoot AllPosts)

defaultIndexPages :: (MonadAction m,
                      MonadThrow m,
                      Indexable xs (Record Stage1Post),
                      IsIndexOf YearMonth xs,
                      IsIndexOf Tag xs,
                      IsIndexOf Posted xs)
                  => IxSet xs (Record Stage1Post)
                  -> Int
                  -> IndexPages PostsFilter
                  -> m [Record (FUrl : FItems Stage1Post : FPageNo : '[])]
defaultIndexPages postIx postsPerPage (IndexPages x) = do
        r <- askOracle $ IndexRoot x
        let k = Ix.toDescList (Proxy @Posted) . indexFilter x $ postIx
        p <- paginate' postsPerPage k
        return $ unzipper $ extend (\a -> r <> "pages/" <> T.pack (show $ pos a + 1) :*: extract a :*: pos a + 1:*: RNil) p
