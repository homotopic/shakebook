{- |
   Module     : Shakebook.Conventions
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Conventions used in Shakebook projects, common lenses, generators, and indexing wrappers over Values.
-}
module Shakebook.Conventions (
  -- * Lenses
  viewImage
, viewModified
, viewPostTime
, viewPostTimeRaw
, viewTags
, viewTitle
, viewAllPostTags
, viewAllPostTimes
, withBaseUrl
, withFullUrl
, withHighlighting
, withModified
, withNext
, withPages
, withPrettyDate
, withPrevious
, withPosts
, withRecentPosts
, withSocialLinks
, withSiteTitle
, withSubsections
, withTagIndex
, withTagLinks
, withTeaser
, withTitle

  -- * Enrichments
, enrichPrettyDate
, enrichTagLinks
, enrichTeaser

  -- * Extensions
, extendNext
, extendPrevious
, extendNextPrevious
, extendPageNeighbours

  -- * Generations
, genBlogNavbarData
, genIndexPageData
, genLinkData
, genPageData
, genTocNavbarData

  -- * Indexing
, Post(..)
, Tag(..)
, Posted(..)
, YearMonth(..)
, SrcFile(..)
, postIndex
, postZipper
, fromYearMonthPair
, toYearMonthPair
) where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Zipper.Extra
import           Control.Lens                 hiding ((:<), Indexable)
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.Aeson.With
import           Data.Hashable.Time
import           Data.IxSet.Typed             as Ix
import           Data.IxSet.Typed.Conversions as Ix
import           Data.Text.Time
import           Development.Shake.Plus
import           RIO                          hiding (view)
import           RIO.List
import           RIO.List.Partial
import qualified RIO.HashMap                  as HM
import qualified RIO.Text                     as T
import qualified RIO.Text.Partial             as T
import           RIO.Time
import qualified RIO.Vector                   as V
import           Shakebook.Pandoc
import           Text.Pandoc.Highlighting

-- | View the "image" field of a JSON value.
viewImage :: ToJSON a => a -> Text
viewImage = view' (key "image" . _String)

-- | View the "modified" field of a JSON value.
viewModified :: ToJSON a => a -> UTCTime
viewModified = parseISODateTime . view' (key "modified" . _String)

-- | View the "date" field of a JSON Value as a UTCTime.
viewPostTime :: ToJSON a => a -> UTCTime
viewPostTime = parseISODateTime . view' (key "date" . _String)

-- | View the "date" field of a JSON Value as Text.
viewPostTimeRaw :: ToJSON a => a -> Text
viewPostTimeRaw = view' (key "date" . _String)

-- | View the "tags" field of a JSON Value as a list.
viewTags :: ToJSON a => a -> [Text]
viewTags = toListOf' (key "tags" . values . _String)

-- | View the "title" field of a JSON Value.
viewTitle :: ToJSON a => a -> Text
viewTitle = view' (key "title" . _String)

-- | View all post tags for a list of posts.
viewAllPostTags :: ToJSON a => [a] -> [Text]
viewAllPostTags = (>>= viewTags)

-- | View all posts times for a list of posts.
viewAllPostTimes :: ToJSON a => [a] -> [UTCTime]
viewAllPostTimes = fmap viewPostTime

-- | Add "base-url" field from input Text.
withBaseUrl :: Text -> Value -> Value
withBaseUrl = withStringField "base-url"

-- | Add "full-url" field  from input Text.
withFullUrl :: Text -> Value -> Value
withFullUrl = withStringField "full-url"

-- | Add "highlighting-css" field from input Style.
withHighlighting :: Style -> Value -> Value
withHighlighting = withStringField "highlighting-css" . T.pack . styleToCss

-- | Add "modified" field from input UTCTime.
withModified :: UTCTime -> Value -> Value
withModified = withStringField "modified" . T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

-- | Add "next" field from input Value.
withNext :: ToJSON a => a -> Value -> Value
withNext = withValue "next"

-- | Add "pages" field from input [Value].
withPages :: ToJSON a => [a] -> Value -> Value
withPages = withArrayField "pages"

-- | Add "prettydate" field using input Text.
withPrettyDate :: Text -> Value -> Value
withPrettyDate = withStringField "pretty-date"

-- | Add "previous" field using input Value.
withPrevious :: ToJSON a => a -> Value -> Value
withPrevious = withValue "previous"

-- | Add "posts" field based on input [Value].
withPosts :: ToJSON a => [a] -> Value -> Value
withPosts = withArrayField "posts"

-- | Add "recent-posts" field using input Value.
withRecentPosts :: ToJSON a => [a] -> Value -> Value
withRecentPosts = withArrayField "recent-posts"

-- | Add "site-title" field from input Text.
withSiteTitle :: Text -> Value -> Value
withSiteTitle = withStringField "site-title"

-- | Add "social-links" field based on input [Value].
withSocialLinks :: ToJSON a => [a] -> Value -> Value
withSocialLinks = withArrayField "social-links"

-- | Add "subsections" field based on input [Value].
withSubsections :: ToJSON a => [a] -> Value -> Value
withSubsections = withArrayField "subsections"

-- | Add "tag-index" field based on input [Value].
withTagIndex :: ToJSON a => [a] -> Value -> Value
withTagIndex = withArrayField "tag-index"

-- | Add "tag-links" field based on input [Value].
withTagLinks :: ToJSON a => [a] -> Value -> Value
withTagLinks  = withArrayField "tag-links"

-- | Add "teaser" field based on input Text.
withTeaser :: Text -> Value -> Value
withTeaser = withStringField "teaser"

-- | Add "title" field based on input Text.
withTitle :: Text -> Value -> Value
withTitle = withStringField "title"

-- | Assuming a "date" field, enrich using withPrettyDate and a format string.
enrichPrettyDate :: (UTCTime -> Text) -> Value -> Value
enrichPrettyDate f v = withPrettyDate (f . viewPostTime $ v) v

-- | Assuming a "tags" field, enrich using withTagLinks.
enrichTagLinks :: (Text -> Text) -> Value -> Value
enrichTagLinks f v = withTagLinks ((genLinkData <*> f) <$> viewTags v) v

-- | Assuming a "content" field with a spitter section, enrich using withTeaser
enrichTeaser :: Text -> Value -> Value
enrichTeaser s v = withTeaser (head (T.splitOn s (viewContent v))) v

-- | Add both "next" and "previous" fields using `withPostNext` and `withPostPrevious`
extendNextPrevious :: Zipper [] Value -> Zipper [] Value
extendNextPrevious  = extendPrevious . extendNext

-- | Extend a Zipper of Values to add "previous" objects.
extendPrevious :: Zipper [] Value -> Zipper [] Value
extendPrevious = extend (liftA2 withPrevious zipperPreviousMaybe extract)

-- | Extend a Zipper of Values to add "next" objects.
extendNext :: Zipper [] Value -> Zipper [] Value
extendNext = extend (liftA2 withNext zipperNextMaybe extract)

-- | Extend a Zipper of Values to add list of "pages" within r hops either side of the focus.
extendPageNeighbours :: Int -> Zipper [] Value -> Zipper [] Value
extendPageNeighbours r = extend (liftA2 withPages (zipperWithin r) extract)

-- | Create link data object with fields "id" and "url" using an id and a function
-- | transforming an id into a url.
genLinkData :: Text -> Text -> Value
genLinkData x u = object ["id" A..= String x, "url" A..= String u]

-- | Indexable Post Type
newtype Post = Post { unPost :: Value }
  deriving (Show, Eq, Ord, Data, Typeable, Hashable, ToJSON)

-- | Tag indices for a `Post` for use with `IxSet`.
newtype Tag = Tag Text
  deriving (Show, Eq, Ord, Data, Typeable, Hashable)

-- | Posted index for a `Post` for use with `IxSet`.
newtype Posted = Posted UTCTime
  deriving (Show, Eq, Ord, Data, Typeable, Hashable)

-- | YearMonth (yyyy, mm) index for a `Post` for use with `IxSet`.
newtype YearMonth = YearMonth (Integer, Int)
  deriving (Show, Eq, Ord, Data, Typeable, Hashable)

-- | SrcFile index for a `Post` for use with `IxSet`.
newtype SrcFile = SrcFile Text
  deriving (Show, Eq, Ord, Data, Typeable, Hashable)

instance Indexable '[Tag, Posted, YearMonth, SrcFile] Post where
  indices = ixList (ixFun (fmap Tag . viewTags))
                   (ixFun (pure . Posted . viewPostTime))
                   (ixFun (pure . YearMonth . toYearMonthPair . viewPostTime))
                   (ixFun (pure . SrcFile . viewSrcPath))

toYearMonthPair :: UTCTime -> (Integer, Int)
toYearMonthPair = (\(a, b, _) -> (a, b)) . toGregorian . utctDay

fromYearMonthPair :: (Integer, Int) -> UTCTime
fromYearMonthPair (y,m) = UTCTime (fromGregorian y m 1) 0

-- | Take a Value loading function and a filepattern and return an indexable set of Posts.
postIndex :: MonadAction m
          => (Within Rel (Path Rel File) -> m Value)
          -> Within Rel [FilePattern]
          -> m (Ix.IxSet '[Tag, Posted, YearMonth, SrcFile] Post)
postIndex rd fp = do
  xs <- batchLoadWithin' fp rd
  return (Ix.fromList $ Post <$> HM.elems xs)

-- | Create a `Zipper [] Post` from an `IxSet xs Post` by ordering by `Posted`.
postZipper :: (MonadThrow m, Ix.IsIndexOf Posted xs) => Ix.IxSet xs Post -> m (Zipper [] Post)
postZipper = Ix.toZipperDesc (Proxy :: Proxy Posted)

-- | Create a blog navbar object for a posts section, with layers "toc1", "toc2", and "toc3".
genBlogNavbarData :: IsIndexOf YearMonth ixs => Text -- ^ "Top level title, e.g "Blog"
                  -> Text -- ^ Root page, e.g "/posts"
                  -> (UTCTime -> Text) -- ^ Formatting function to a UTCTime to a title.
                  -> (UTCTime -> Text) -- ^ Formatting function to convert a UTCTime to a URL link
                  -> IxSet ixs Post
                  -> Value
genBlogNavbarData a b f g xs = object [ "toc1" A..= object [
                                        "title" A..= String a
                                      , "url"   A..= String b
                                      , "toc2"  A..= Array (V.fromList $ map (uncurry toc2) $ groupDescBy xs)]
                                     ] where
       toc2 _ [] = object []
       toc2 (YearMonth (_, _)) t@(x : _) = object [ "title" A..= String (f (viewPostTime x))
                                                  , "url"   A..= String (g (viewPostTime x))
                                                  , "toc3"  A..= Array (V.fromList $ sortOn (Down . viewPostTime) (unPost <$> t)) ]

-- | Create a toc navbar object for a docs section, with layers "toc1", "toc2" and "toc3".
genTocNavbarData :: Cofree [] Value -> Value
genTocNavbarData (x :< xs) =
  object ["toc1" A..= [_Object . at "toc2" ?~ Array (V.fromList $ map toc2 xs) $ x]] where
      toc2 (y :< ys) = (_Object . at "toc3" ?~ Array (V.fromList $ map extract ys)) y

genPageData :: ToJSON a => Text -> (Text -> Text) -> Zipper [] [a] -> Value
genPageData t f xs = let x = T.pack . show $ pos xs + 1
                     in withTitle t
                      . withJSON (genLinkData x (f x))
                      . withPosts (extract xs) $ Object mempty

genIndexPageData :: (MonadThrow m, ToJSON a)
                 => Text
                 -> (Text -> Text)
                 -> Int
                 -> [a]
                 -> m (Zipper [] Value)
genIndexPageData g h n xs = do
 zs <- paginate' n $ sortOn (Down . viewPostTime) xs
 return $ extend (genPageData g h) zs
