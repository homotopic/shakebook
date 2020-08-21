module Shakebook.Lucid where

import           Control.Comonad.Cofree
import           Control.Comonad.Zipper.Extra
import           Data.IxSet.Typed             as Ix
import           Development.Shake.Plus
import           Lucid
import           Lucid.Base
import           RIO
import           RIO.List
import qualified RIO.Text                     as T
import qualified RIO.Text.Lazy                as LT
import           Text.Pandoc.Highlighting

newtype HtmlFragment = HtmlFragment { unHtmlFragment :: Text }
  deriving (Eq, Show, Generic, NFData, Binary, Hashable, ToHtml)

toHtmlFragment :: Html () -> HtmlFragment
toHtmlFragment = HtmlFragment . LT.toStrict . renderText

toHtmlFragmentM :: Monad m => HtmlT m () -> m HtmlFragment
toHtmlFragmentM = fmap toHtmlFragment . commuteHtmlT

renderLink :: Monad m => Text -> Text -> HtmlT m ()
renderLink x y = a_ [href_ $ y] (toHtml x)

renderCofree :: (Monad m) => (a -> HtmlT m ()) -> Cofree [] a -> HtmlT m ()
renderCofree f (x :< []) = f x
renderCofree f (x :< xs) = f x >> ul_ (forM_ xs (li_ . renderCofree f))

renderZipperWithin :: Monad m => (a -> HtmlT m ()) -> Int -> Zipper [] a -> HtmlT m ()
renderZipperWithin f a xs = mapM_ f (zipperWithin a xs)

renderIxSetGroupDescBy :: (Ord b, Ix.IsIndexOf ix ixs, Monad m)
                       => (ix -> HtmlT m ())
                       -> (a  -> HtmlT m ())
                       -> (a  -> b)
                       -> Ix.IxSet ixs a
                       -> HtmlT m ()
renderIxSetGroupDescBy f g h as = ul_ $ forM_ (Ix.groupDescBy as) $ \(y, xs) -> li_ $ do
                                      f y
                                      ul_ $ forM_ (sortOn h xs) $ li_ . g

newtype StyleFragment = StyleFragment { unStyleFragment :: Text }
  deriving (Eq, Show, Generic, NFData, Binary, Hashable, ToHtml)

toStyleFragment :: Style -> StyleFragment
toStyleFragment = StyleFragment . T.pack . styleToCss

