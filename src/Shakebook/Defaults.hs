{- |
   Module     : Shakebook.Defaults
   License    : MIT
   Stability  : experimental

Seom sensible default settings for certain shakebook functions.
-}
module Shakebook.Defaults where

import           Lucid.CDN
import           RIO
import           RIO.List.Partial
import qualified RIO.Map                  as M
import qualified RIO.Text                 as T
import qualified RIO.Text.Partial         as T
import           RIO.Time
import           Shakebook.Lucid
import           Text.DocTemplates
import           Text.Pandoc.Definition
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options

defaultMonthUrlFormat :: UTCTime -> Text
defaultMonthUrlFormat = T.pack . formatTime defaultTimeLocale "%Y-%m"

defaultPrettyMonthFormat :: UTCTime -> Text
defaultPrettyMonthFormat = T.pack . formatTime defaultTimeLocale "%B, %Y"

defaultPrettyTimeFormat :: UTCTime -> Text
defaultPrettyTimeFormat = T.pack . formatTime defaultTimeLocale "%A, %B %d, %Y"

defaultHighlighting :: StyleFragment
defaultHighlighting = toStyleFragment pygments

defaultMarkdownReaderOptions :: ReaderOptions
defaultMarkdownReaderOptions = def { readerExtensions = pandocExtensions }

defaultHtml5WriterOptions :: WriterOptions
defaultHtml5WriterOptions = def { writerHTMLMathMethod = MathJax ""}

defaultLatexWriterOptions :: WriterOptions
defaultLatexWriterOptions = def { writerTableOfContents = True
                                , writerVariables = Context $ M.fromList [
                                               ("geometry", SimpleVal "margin=3cm")
                                             , ("fontsize", SimpleVal "10")
                                             , ("linkcolor",SimpleVal "blue")]
                                }

defaultVideoReplacement :: Text -> Text -> Inline
defaultVideoReplacement baseUrl x = Str $ "[Video available at [" <> baseUrl <> "/" <> x <> "]"

defaultDeriveTeaser :: Text -> Text
defaultDeriveTeaser = head . T.splitOn "<!-- more -->"

defaultCdnImports :: HtmlFragment
defaultCdnImports = toHtmlFragment $ do
  bootstrapCSS_5_0_0_alpha1
  bootstrapJS_5_0_0_alpha1
  fontawesome_4_7_0
  mathjax_3_1_0
  jquery_3_5_1
  popper_2_4_4
