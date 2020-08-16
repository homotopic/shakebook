module Shakebook.Lucid where

import Control.Comonad.Cofree
import Development.Shake.Plus
import Lucid
import Lucid.Base
import qualified RIO.Text.Lazy as LT
import qualified RIO.Text as T
import RIO
import Text.Pandoc.Highlighting
import Control.Comonad.Zipper.Extra

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

newtype StyleFragment = StyleFragment { unStyleFragment :: Text }
  deriving (Eq, Show, Generic, NFData, Binary, Hashable, ToHtml)

toStyleFragment :: Style -> StyleFragment
toStyleFragment = StyleFragment . T.pack . styleToCss

