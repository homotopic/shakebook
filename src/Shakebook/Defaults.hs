{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Defaults where

import           Data.Aeson                   as A
import           RIO
import qualified RIO.Map                      as M
import qualified RIO.Text                     as T
import           RIO.Time
import           Path
import           Shakebook.Conventions
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

defaultPagePaths :: MonadThrow m => [Int] -> m [Path Rel File]
defaultPagePaths xs = do
  xs' <- mapM (parseRelDir . show) xs
  return $ fmap (\i -> $(mkRelDir "pages") </> i </> $(mkRelFile "index.html")) xs'
