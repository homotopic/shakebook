{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Defaults where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Store.Class
import           Control.Comonad.Zipper.Extra
import           Control.Monad.Extra
import           Data.Aeson                   as A
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake.Plus
import qualified Development.Shake.FilePath   as S
import           RIO
import qualified RIO.ByteString.Lazy          as LBS
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Map                      as M
import           RIO.Partial
import qualified RIO.Text                     as T
import           RIO.Time
import           Path                         as P
import           Shakebook.Aeson
import           Shakebook.Conventions
import           Shakebook.Data
import           Shakebook.Mustache
import           Text.DocTemplates
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.PDF
import           Text.Pandoc.Readers
import           Text.Pandoc.Templates
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers
import           Within

defaultMonthUrlFormat :: UTCTime -> String
defaultMonthUrlFormat = formatTime defaultTimeLocale "%Y-%m"

defaultPrettyMonthFormat :: UTCTime -> String
defaultPrettyMonthFormat = formatTime defaultTimeLocale "%B, %Y"

defaultPrettyTimeFormat :: UTCTime -> String
defaultPrettyTimeFormat = formatTime defaultTimeLocale "%A, %B %d, %Y"

defaultIndexFileFragment :: Path Rel File
defaultIndexFileFragment = $(mkRelFile "index.html")

defaultMonthDirFragment :: MonadThrow m => UTCTime -> m (Path Rel Dir)
defaultMonthDirFragment t = do
  k <- parseRelDir $ defaultMonthUrlFormat t
  return $ $(mkRelDir "posts/months") </> k

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

defaultSbSrcDir :: Path Rel Dir
defaultSbSrcDir = $(mkRelDir "site")

defaultSbOutDir :: Path Rel Dir
defaultSbOutDir = $(mkRelDir "public")

defaultPostsPerPage :: Int
defaultPostsPerPage = 5

defaultSbConfig :: Text -- ^ BaseURL
                -> Text -- ^ SiteTitle
                -> SbConfig
defaultSbConfig x y = SbConfig
  defaultSbSrcDir
  defaultSbOutDir
  x
  defaultMarkdownReaderOptions
  defaultHtml5WriterOptions
  defaultPostsPerPage
  (withSiteTitle y)
