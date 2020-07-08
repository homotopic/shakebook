{- |
   Module     : Shakebook.Defaults
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Seom sensible default settings for certain shakebook functions.
-}

{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Defaults where

import           Data.Aeson             as A
import           Development.Shake.Plus
import           Lucid
import           Lucid.CDN
import           RIO
import qualified RIO.Map                as M
import qualified RIO.Text               as T
import           RIO.Time
import           Shakebook.Conventions
import           Text.DocTemplates
import           Text.Pandoc.Definition
import           Text.Pandoc.Options

defaultMonthUrlFormat :: UTCTime -> Text
defaultMonthUrlFormat = T.pack . formatTime defaultTimeLocale "%Y-%m"

defaultPrettyMonthFormat :: UTCTime -> Text
defaultPrettyMonthFormat = T.pack . formatTime defaultTimeLocale "%B, %Y"

defaultPrettyTimeFormat :: UTCTime -> Text
defaultPrettyTimeFormat = T.pack . formatTime defaultTimeLocale "%A, %B %d, %Y"

defaultMonthUrlFragment :: UTCTime -> Text
defaultMonthUrlFragment t = "/posts/months/" <> defaultMonthUrlFormat t

defaultEnrichPost :: Value -> Value
defaultEnrichPost = enrichTeaser "<!--more-->"
                  . enrichTagLinks ("/posts/tags/" <>)
                  . enrichPrettyDate defaultPrettyTimeFormat

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

defaultPagePaths :: MonadThrow m => [Int] -> m [Path Rel File]
defaultPagePaths xs = do
  xs' <- mapM (parseRelDir . show) xs
  return $ fmap (\i -> $(mkRelDir "pages") </> i </> $(mkRelFile "index.html")) xs'

defaultCdnImports :: Monad m => HtmlT m ()
defaultCdnImports = do
  bootstrapCSS_5_0_0_alpha1
  bootstrapJS_5_0_0_alpha1
  fontawesome_4_7_0
  mathjax_3_0_5
  jquery_3_5_1
  popper_2_4_4
