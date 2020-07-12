{-# LANGUAGE TemplateHaskell #-}
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

  -- * Lenses
, viewImage
, viewModified
, viewPosted
, viewTags
, viewTitle
, viewUrl
, viewSrcPath

  -- * Generations
, genBlogNavbarData
, genIndexPageData
, genTocNavbarData
, addDerivedUrl

  -- * Indexing
, Link
, Tag(..)
, Posted(..)
, YearMonth(..)
, SrcFile(..)
, postIndex
, postZipper
, fromYearMonthPair
, toYearMonthPair
, RawIndexPage
, toGroundedUrl
) where

import Composite.Record
import Composite.Aeson
import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Zipper.Extra
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.Aeson.With
import           Data.Hashable.Time
import           Data.IxSet.Typed             as Ix
import           Data.IxSet.Typed.Conversions as Ix
import           Data.Text.Time
import           Development.Shake.Plus hiding ((:->))
import           Lucid
import           RIO                          
import           RIO.List
import           RIO.List.Partial
import qualified RIO.HashMap                  as HM
import qualified RIO.Text                     as T
import qualified RIO.Text.Lazy                as LT
import qualified RIO.Text.Partial             as T
import           RIO.Time
import           Shakebook.Pandoc
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
type FPages         = "pages"        :-> Html ()
type FPrettyDate    = "prettydate"   :-> UTCTime
type FPrevious      = "previous"     :-> Html ()
type FPosted        = "posted"       :-> UTCTime
type FPosts x       = "posts"        :-> [Record x]
type FRecentPosts x = "recent-posts" :-> [Record x]
type FSiteTitle     = "site-title"   :-> Text
type FSrcPath       = "src-path"     :-> Path Rel File
type FSocial        = "social"       :-> [Record Link]
type FTags          = "tags"         :-> [Text]
type FTagLinks      = "tag-links"    :-> [Record Link]
type FToc           = "toc"          :-> Html ()
type FSubsections x = "subsections"  :-> [Record x]
type FTeaser        = "teaser"       :-> Text
type FTitle         = "title"        :-> Text
type FUrl           = "url"          :-> Text

type Link = '[FId, FUrl]

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

instance (Ord (Record xs), RElem FTags xs, RElem FPosted xs, RElem FSrcPath xs) => Indexable '[Tag, Posted, YearMonth, SrcFile] (Record xs) where
  indices = ixList (ixFun (fmap Tag . viewTags))
                   (ixFun (pure . Posted . viewPosted))
                   (ixFun (pure . YearMonth . toYearMonthPair . viewPosted))
                   (ixFun (pure . SrcFile . T.pack . toFilePath . toFile . viewSrcPath))

toYearMonthPair :: UTCTime -> (Integer, Int)
toYearMonthPair = (\(a, b, _) -> (a, b)) . toGregorian . utctDay

fromYearMonthPair :: (Integer, Int) -> UTCTime
fromYearMonthPair (y,m) = UTCTime (fromGregorian y m 1) 0

-- | Take a Value loading function and a filepattern and return an indexable set of Posts.
postIndex :: (MonadAction m, Indexable '[Tag, Posted, YearMonth, SrcFile] (Record xs))
          => (Within Rel (Path Rel File) -> m (Record xs))
          -> Within Rel [FilePattern]
          -> m (Ix.IxSet '[Tag, Posted, YearMonth, SrcFile] (Record xs))
postIndex rd fp = do
  xs <- batchLoadWithin' fp rd
  return (Ix.fromList $ HM.elems xs)

-- | Create a `Zipper [] Post` from an `IxSet xs Post` by ordering by `Posted`.
postZipper :: (MonadThrow m, Ix.IsIndexOf Posted ixs) => Ix.IxSet ixs (Record xs) -> m (Zipper [] (Record xs))
postZipper = Ix.toZipperDesc (Proxy :: Proxy Posted)

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
      a_ [href_ a] (toHtml b)
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

genIndexPageData :: (MonadThrow m, RElem FPosted xs)
                 => Text
                 -> (Int -> Text)
                 -> Int
                 -> [Record xs]
                 -> m (Zipper [] (Record (RawIndexPage xs)))
genIndexPageData g h n xs = do
 zs <- paginate' n $ sortOn (Down . viewPosted) xs
 return $ extend (\x -> h (pos x) :*: g :*: extract x :*: RNil) zs

addDerivedUrl :: (MonadThrow m, RElem FSrcPath xs) => (Path Rel File -> m Text) -> Record xs -> m (Record (FUrl : xs))
addDerivedUrl f xs = f (viewSrcPath xs) >>= \x -> return $ x :*: xs

-- | Add a leading slash to a `Path Rel File` to turn it into a url as `Text`.
toGroundedUrl :: Path Rel File -> Text
toGroundedUrl = T.pack . toFilePath . ($(mkAbsDir "/") </>)
