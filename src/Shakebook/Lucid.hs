module Shakebook.Lucid where

import Data.Binary
import Development.Shake.Plus
import Lucid
import qualified RIO.Text.Lazy as LT
import qualified RIO.Text as T
import RIO
import Text.Pandoc.Highlighting

newtype HtmlFragment = HtmlFragment { unHtmlFragment :: Text }
  deriving (Eq, Show, Generic, NFData, Binary, Hashable, ToHtml)

toHtmlFragment :: Html () -> HtmlFragment
toHtmlFragment = HtmlFragment . LT.toStrict . renderText

newtype StyleFragment = StyleFragment { unStyleFragment :: Text }
  deriving (Eq, Show, Generic, NFData, Binary, Hashable, ToHtml)

toStyleFragment :: Style -> StyleFragment
toStyleFragment = StyleFragment . T.pack . styleToCss
