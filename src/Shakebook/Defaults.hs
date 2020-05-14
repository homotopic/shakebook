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
import           Text.DocTemplates
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.PDF
import           Text.Pandoc.Readers
import           Text.Pandoc.Templates
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers

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


makePDFLaTeX :: Pandoc -> PandocIO (Either LBS.ByteString LBS.ByteString)
makePDFLaTeX p = do
  t <- compileDefaultTemplate "latex"
  makePDF "pdflatex" [] writeLaTeX defaultLatexWriterOptions { writerTemplate = Just t } p

handleImages :: Text -> (Text -> Text) -> Inline -> Inline
handleImages prefix f (Image attr ins (src,txt)) =
  if T.takeEnd 4 src == ".mp4" then Str (f src)
  else Image attr ins (prefix <> "/" <> src, txt)
handleImages _ _ x = x

handleHeaders :: Int -> Block -> Block
handleHeaders i (Header a as xs) = Header (max 1 (a + i)) as xs
handleHeaders _ x                = x

pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

--- | Build a PDF from a Cofree table of contents.
buildPDF :: (MonadShakebookAction r m, MonadFail m) => Cofree [] (Path Rel File) -> Path Rel File -> FilePath -> m ()
buildPDF toc meta out = view sbConfigL >>= \SbConfig {..} -> do
  y <- mapM (readFileIn' sbSrcDir) toc
  m <- readFileIn' sbSrcDir meta
  Right f <- liftIO . runIOorExplode $ do
    k <- mapM (readMarkdown sbMdRead ) y
    a <- readMarkdown sbMdRead $ m
    let z = walk (handleImages (T.pack $ toFilePath sbOutDir) (\x -> "[Video available at " <> sbBaseUrl <> x <> "]")) $ foldr (<>) a $ pushHeaders (-1) k
    makePDFLaTeX z
  LBS.writeFile out f


