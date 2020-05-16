{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Defaults where

import           Control.Comonad.Cofree
import           Control.Monad.Extra
import           Data.Aeson                   as A
import           Development.Shake.Plus
import           RIO
import qualified RIO.ByteString.Lazy          as LBS
import qualified RIO.Map                      as M
import qualified RIO.Text                     as T
import           RIO.Time
import           Path                         as P
import           Shakebook.Conventions
import           Shakebook.Data
import           Shakebook.Pandoc
import           Text.DocTemplates
import           Text.Pandoc.Definition
import           Text.Pandoc.Options

defaultMonthUrlFormat :: UTCTime -> String
defaultMonthUrlFormat = formatTime defaultTimeLocale "%Y-%m"

defaultPrettyMonthFormat :: UTCTime -> String
defaultPrettyMonthFormat = formatTime defaultTimeLocale "%B, %Y"

defaultPrettyTimeFormat :: UTCTime -> String
defaultPrettyTimeFormat = formatTime defaultTimeLocale "%A, %B %d, %Y"

defaultMonthUrlFragment :: UTCTime -> Text
defaultMonthUrlFragment t = T.pack $ "/posts/months/" <> defaultMonthUrlFormat t

defaultEnrichPost :: Value -> Value
defaultEnrichPost = enrichTeaser "<!--more-->"
                  . enrichTagLinks ("/posts/tags/" <>)
                  . enrichPrettyDate defaultPrettyTimeFormat
--                  . enrichTypicalUrl

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
defaultVideoReplacement baseUrl = \x -> Str $ "[Video available at [" <> baseUrl <> "/" <> x <> "]"

--- | Build a PDF from a Cofree table of contents.
buildPDF :: (MonadThrow m, MonadAction m, MonadReader r m, HasSbConfig r) => Cofree [] (Path Rel File) -> Path Rel File -> FilePath -> m ()
buildPDF toc meta out = view sbConfigL >>= \SbConfig {..} -> do
  k <- mapM (readMDFileIn sbMdRead sbSrcDir) toc
  a <- readMDFileIn sbMdRead sbSrcDir meta
  z <- replaceUnusableImages [".mp4"] (defaultVideoReplacement sbBaseUrl) $ foldr (<>) a $ progressivelyDemoteHeaders k
  needPandocImagesIn sbOutDir z
  let z' = prefixAllImages sbOutDir z
  f <- makePDFLaTeX defaultLatexWriterOptions z'
  either (throwM . PandocActionException . show) (LBS.writeFile out) f
