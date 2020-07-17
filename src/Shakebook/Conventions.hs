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
  FContent
, FPageLinks
, FPosted
, FSiteTitle
, FTitle
, FUrl
, FTags
, FImage
, FCdnImports
, FHighlighting
, FDescription
, FModified
, FSubsections
, FSocial
, FSrcPath
, FTagLinks
, FToc
, FTeaser
, FRecentPosts
, FPrettyDate
, FNext
, FPrevious
, FItems

  -- * Lenses
, viewContent
, viewImage
, viewModified
, viewPosted
, viewTags
, viewTitle
, viewUrl
, viewSrcPath

  -- * Generations
, genBlogNavbarData
, genTocNavbarData
, addDerivedUrl

  -- * Indexing
, Link
, Tag(..)
, Posted(..)
, YearMonth(..)
, batchLoadIndex
, fromYearMonthPair
, toYearMonthPair
, RawIndexPage
, toGroundedUrl
, fromGroundedUrlF
, fromGroundedUrlD

  -- * Stages
, RawPost
, Stage1Post
, RawDoc
, Stage1Doc
, PostSet

  -- * Formatting
, rawDocJsonFormat
, rawPostJsonFormat
, rawSingleJsonFormat
, stage1PostJsonFormat
, stage1DocJsonFormat
, finalPostJsonFormat
, mainPageJsonFormat
, indexPageJsonFormat
, finalDocJsonFormat

  -- * Templates
, TMain
, TDoc
, TPost
, TPostIndex
, Enriched
) where

import           Composite.Aeson
import           Composite.Record
import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Data.Hashable.Time
import           Data.IxSet.Typed             as Ix
import           Development.Shake.Plus       hiding ((:->))
import           Lucid
import           RIO
import qualified RIO.HashMap                  as HM
import           RIO.List
import qualified RIO.Text                     as T
import           RIO.Time
import           Shakebook.Aeson
import           Text.Pandoc.Highlighting

type FCdnImports    = "cdn-imports"  :-> Html ()
type FContent       = "content"      :-> Text
type FDescription   = "description"  :-> Text
type FElements x    = "elements"     :-> [Record x]
type FHighlighting  = "highlighting" :-> Style
type FImage         = "image"        :-> Maybe Text
type FId            = "id"           :-> Text
type FModified      = "modified"     :-> UTCTime
type FNext          = "next"         :-> Html ()
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

type Link = '[FId, FUrl]

-- | View the "image" field of a JSON value.
viewContent :: RElem FContent xs => Record xs -> Text
viewContent = view (rlens (Proxy @FContent))

-- | View the "image" field of a JSON value.
viewImage :: RElem FImage xs => Record xs -> Maybe Text
viewImage = view (rlens (Proxy @FImage))

-- | View the "modified" field of a JSON value.
viewModified :: RElem FModified xs => Record xs -> UTCTime
viewModified = view (rlens (Proxy @FModified))

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
  deriving (Show, Eq, Ord, Data, Typeable, Hashable, Binary, NFData)

-- | Posted index for a `Post` for use with `IxSet`.
newtype Posted = Posted UTCTime
  deriving (Show, Eq, Ord, Data, Typeable, Hashable, NFData)

-- | YearMonth (yyyy, mm) index for a `Post` for use with `IxSet`.
newtype YearMonth = YearMonth (Integer, Int)
  deriving (Show, Eq, Ord, Data, Typeable, Hashable, Binary, NFData)

toYearMonthPair :: UTCTime -> (Integer, Int)
toYearMonthPair = (\(a, b, _) -> (a, b)) . toGregorian . utctDay

fromYearMonthPair :: (Integer, Int) -> UTCTime
fromYearMonthPair (y,m) = UTCTime (fromGregorian y m 1) 0

-- | Take a Value loading function and a filepattern and return an indexable set of Posts.
batchLoadIndex :: (MonadAction m, Indexable ixs x)
          => (Path Rel File -> m x)
          -> Path Rel Dir
          -> [FilePattern]
          -> m (Ix.IxSet ixs x)
batchLoadIndex rd dir fp = do
  xs <- batchLoad dir fp rd
  return (Ix.fromList $ HM.elems xs)

-- | Create a blog navbar object for a posts section, with layers "toc1", "toc2", and "toc3".
genBlogNavbarData :: (IsIndexOf YearMonth ixs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs)
                  => Text -- ^ "Top level title, e.g "Blog"
                  -> Text -- ^ Root page, e.g "/posts"
                  -> (UTCTime -> Text) -- ^ Formatting function to a UTCTime to a title.
                  -> (UTCTime -> Text) -- ^ Formatting function to convert a UTCTime to a URL link
                  -> IxSet ixs (Record xs)
                  -> Html ()
genBlogNavbarData a b f g xs =
  ul_ $
    li_ $ do
      a_ [href_ b] (toHtml a)
      ul_ $ forM_ (groupDescBy xs) $ \(YearMonth (y, m), xs') -> do
        let t' = fromYearMonthPair (y, m)
        li_ $ a_ [href_ $ g t'] (toHtml $ f t')
        ul_ $ forM (sortOn (Down . view (rlens (Proxy @FPosted))) xs') $ \x ->
          li_ $ a_ [href_ $ view (rlens (Proxy @FUrl)) x] (toHtml $ view (rlens (Proxy @FTitle)) x)

-- | Create a toc navbar object for a docs section, with layers "toc1", "toc2" and "toc3".
genTocNavbarData :: (RElem FUrl xs, RElem FTitle xs) => Cofree [] (Record xs) -> Html ()
genTocNavbarData (x :< xs) =
  ul_ $
    li_ $ do
      a_ [href_ $ view (rlens (Proxy @FUrl)) x] (toHtml $ view (rlens (Proxy @FTitle)) x)
      forM_ xs genTocNavbarData

type RawIndexPage x = '[FUrl, FTitle, FElements x]

addDerivedUrl :: (MonadThrow m, RElem FSrcPath xs) => (Path Rel File -> m Text) -> Record xs -> m (Record (FUrl : xs))
addDerivedUrl f xs = f (viewSrcPath xs) >>= \x -> return $ x :*: xs

-- | Add a leading slash to a `Path Rel File` to turn it into a url as `Text`.
toGroundedUrl :: Path Rel b -> Text
toGroundedUrl = T.pack . toFilePath . ($(mkAbsDir "/") </>)


fromGroundedUrlD :: MonadThrow m => Text -> m (Path Rel Dir)
fromGroundedUrlD x = (parseAbsDir . T.unpack $ x) >>= stripProperPrefix $(mkAbsDir "/")

fromGroundedUrlF :: MonadThrow m => Text -> m (Path Rel File)
fromGroundedUrlF x = (parseAbsFile . T.unpack $ x) >>= stripProperPrefix $(mkAbsDir "/")

--- Stage 0 Types

-- "Basic Markdown" - These two fields are always populated by the markdown loader - the source path and the main body content.
type BasicMD = FSrcPath : FContent : '[]

basicMDJsonFormat :: JsonFormatRecord e BasicMD
basicMDJsonFormat = field relFileJsonFormat
                 :& field textJsonFormat
                 :& RNil

-- A 'RawDoc' contains three mandatory fields - title, description and modified.
type RawDoc = FDescription : FTitle : FModified : BasicMD

rawDocJsonFormat :: JsonFormatRecord e RawDoc
rawDocJsonFormat = field aesonJsonFormat
                :& field defaultJsonFormat
                :& field iso8601DateTimeJsonFormat
                :& basicMDJsonFormat

-- A `RawPost` contains three mandatory fields, title, tags, and posted, and an optional image field.
type RawPost = FTitle : FImage : FTags : FPosted : BasicMD

rawPostJsonFormat :: JsonFormatRecord e RawPost
rawPostJsonFormat = field textJsonFormat
                 :& optionalField textJsonFormat
                 :& field (listJsonFormat textJsonFormat)
                 :& field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%F" "yyyy-mm-dd" :| []))
                 :& basicMDJsonFormat

type RawSingle = FTitle : FImage : BasicMD

rawSingleJsonFormat :: JsonFormatRecord e RawSingle
rawSingleJsonFormat = field textJsonFormat
                   :& optionalField textJsonFormat
                   :& basicMDJsonFormat

type URLised x = FUrl : x

urlisedXJsonFormat :: JsonFormatRecord e x -> JsonFormatRecord e (URLised x)
urlisedXJsonFormat x = field defaultJsonFormat :& x


type Stage1Post = FPrettyDate : FTagLinks : FTeaser : URLised RawPost

stage1PostJsonFormat :: JsonFormatRecord e Stage1Post
stage1PostJsonFormat = field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%A, %B %d, %Y" "yyyy-mm-dd" :| []))
                    :& field (listJsonFormat $ recordJsonFormat linkJsonFormat)
                    :& field textJsonFormat
                    :& urlisedXJsonFormat rawPostJsonFormat

type Stage1Doc = URLised RawDoc

stage1DocJsonFormat :: JsonFormatRecord e Stage1Doc
stage1DocJsonFormat = urlisedXJsonFormat rawDocJsonFormat

--- Stage 2 Types

-- Enrichment provides fields most page templates require.
type Enriched x = FSocial : FCdnImports : FHighlighting : FSiteTitle : x

linkJsonFormat :: JsonFormatRecord e Link
linkJsonFormat = field textJsonFormat :& field textJsonFormat :& RNil

enrichedXJsonFormat :: JsonFormatRecord e x -> JsonFormatRecord e (Enriched x)
enrichedXJsonFormat x = field (listJsonFormat (recordJsonFormat linkJsonFormat))
                     :& field lucidJsonFormat
                     :& field styleJsonFormat
                     :& field defaultJsonFormat
                     :& x

type FinalDoc = FToc : FSubsections Stage1Doc : Enriched Stage1Doc

finalDocJsonFormat :: JsonFormatRecord e FinalDoc
finalDocJsonFormat = field lucidJsonFormat
                  :& field (listJsonFormat $ recordJsonFormat stage1DocJsonFormat)
                  :& enrichedXJsonFormat stage1DocJsonFormat

type FinalPost = FToc : Enriched Stage1Post

finalPostJsonFormat :: JsonFormatRecord e FinalPost
finalPostJsonFormat = field lucidJsonFormat
                   :& enrichedXJsonFormat stage1PostJsonFormat

type IndexPage x = Enriched (FPageLinks : FToc : FItems x : FTitle : '[])

indexPageJsonFormat :: JsonFormat e (Record x) -> JsonFormatRecord e (IndexPage x)
indexPageJsonFormat x = enrichedXJsonFormat $ field (listJsonFormat $ recordJsonFormat linkJsonFormat)
                                           :& field lucidJsonFormat
                                           :& field (listJsonFormat x)
                                           :& field textJsonFormat
                                           :& RNil

type MainPage = FRecentPosts Stage1Post : Enriched (FTitle : FImage : BasicMD)

mainPageJsonFormat :: JsonFormatRecord e MainPage
mainPageJsonFormat = field (listJsonFormat $ recordJsonFormat stage1PostJsonFormat)
                  :& enrichedXJsonFormat (field textJsonFormat
                                       :& optionalField textJsonFormat
                                       :& basicMDJsonFormat)

instance Ix.Indexable '[Tag, Posted, YearMonth] (Record Stage1Post) where
  indices = Ix.ixList (Ix.ixFun (fmap Tag . viewTags))
                    (Ix.ixFun (pure. Posted. viewPosted))
                      (Ix.ixFun (pure . YearMonth . toYearMonthPair . viewPosted))

type PostSet x = Ix.IxSet '[Tag, Posted, YearMonth] (Record x)

type TMain      = "templates/index.html" :-> Record MainPage
type TDoc       = "templates/docs.html"  :-> Record FinalDoc
type TPost      = "templates/post.html"  :-> Record FinalPost
type TPostIndex = "templates/post-list.html" :-> Record (IndexPage Stage1Post)
