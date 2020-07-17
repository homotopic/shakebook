{- |
   Module     : Shakebook.Pandoc
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Pandoc utilities lifted to `MonadAction`.
-}
module Shakebook.Pandoc (
-- * runPandocA
  runPandocA
, PandocActionException(..)

-- * Readers
, readFilePandoc
, readCSVFile
, readLaTeXFile
, readMarkdownFile
, readMediaWikiFile
, loadMarkdownAsJSON

-- * Writers
, makePDFLaTeX

-- * File Rules
, needPandocImagesIn

-- * Filters
, flattenMeta
, prefixAllImages
, progressivelyDemoteHeaders
, replaceUnusableImages
) where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Data.Aeson
import           Data.Aeson.With
import           Development.Shake.Plus hiding ((:->))
import           RIO
import qualified RIO.ByteString.Lazy    as LBS
import qualified RIO.Text               as T
import qualified Slick.Pandoc
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.PDF
import           Text.Pandoc.Readers
import           Text.Pandoc.Templates
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers

newtype PandocActionException = PandocActionException String
    deriving (Show, Eq, Typeable)

instance Exception PandocActionException where
  displayException (PandocActionException s) = s

-- | Natural transformation from `PandocIO` to a `MonadAction`
runPandocA :: (MonadAction m, MonadThrow m ) => PandocIO a -> m a
runPandocA p = do
  result <- liftIO $ runIO p
  either throwM return result

-- | Run a Pandoc reader as a Shake action.
readFilePandoc :: (MonadAction m, MonadThrow m, FileLike b a) => (ReaderOptions -> Text -> PandocIO Pandoc) -> ReaderOptions -> a -> m Pandoc
readFilePandoc run ropts src = readFile' src >>= runPandocA . run ropts

-- | Read a markdown file and return a `Pandoc` as an Action.
readMarkdownFile :: (MonadAction m, MonadThrow m, FileLike b a) => ReaderOptions -> a -> m Pandoc
readMarkdownFile = readFilePandoc readMarkdown

-- | Read a mediawiki file and return a `Pandoc` as an Action.
readMediaWikiFile :: (MonadAction m, MonadThrow m, FileLike b a) => ReaderOptions -> a -> m Pandoc
readMediaWikiFile = readFilePandoc readMediaWiki

-- | Read a LaTeX file and return a `Pandoc` as an Action.
readLaTeXFile :: (MonadAction m, MonadThrow m, FileLike b a) => ReaderOptions -> a -> m Pandoc
readLaTeXFile = readFilePandoc readLaTeX

-- | Read a CSV file and return a `Pandoc` as an Action.
readCSVFile :: (MonadAction m, MonadThrow m, FileLike b a) => ReaderOptions -> a -> m Pandoc
readCSVFile = readFilePandoc readCSV

-- | Find all the images in a `Pandoc` data structure and call `Development.Shake.Plus.need` on them.
needPandocImagesIn :: (MonadAction m, MonadThrow m) => Path Rel Dir -> Pandoc -> m ()
needPandocImagesIn outDir pdoc =
  mapM parseRelFile (drop 1 . T.unpack <$> pullImages pdoc) >>= needIn outDir where
    pullImages = query f
    f (Image _ _ (src, _)) = [src]
    f _                    = []

-- | Make a pdflatex in an `Action`.
makePDFLaTeX :: (MonadAction m, MonadThrow m) => WriterOptions -> Pandoc -> m LBS.ByteString
makePDFLaTeX wopts p = do
  f <- runPandocA $ do
    t <- compileDefaultTemplate "latex"
    makePDF "pdflatex" [] writeLaTeX wopts { writerTemplate = Just t } p
  either (throwM . PandocActionException . show) return f

-- | Precarious function that demotes Header numbers within the Pandoc according to its depth
-- in the Cofree. This is so that Headers that H1s that would correctly display for an HTML page
-- will be lower in the table of contents in the PDF equivalent.
progressivelyDemoteHeaders :: Cofree [] Pandoc -> Cofree [] Pandoc
progressivelyDemoteHeaders = pushHeaders 0 where
  handleHeaders :: Int -> Block -> Block
  handleHeaders i (Header a as xs) = Header (max 1 (a + i)) as xs
  handleHeaders _ x                = x

  pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
  pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

-- | For a list of file extensions, replace the images with an Inline based on its src path.
replaceUnusableImages :: MonadThrow m => [String] -> (Text -> Inline) -> Pandoc -> m Pandoc
replaceUnusableImages exts f = walkM handleImages where
  handleImages i@(Image _ _ (src, _)) = do
    x <- parseAbsFile (T.unpack src) >>= fileExtension
    return $ if x `elem` exts then f src else i
  handleImages x = return x

-- | Prefix all images in a `Pandoc` with a directory.
prefixAllImages :: Path Rel Dir -> Pandoc -> Pandoc
prefixAllImages dir = walk handleImages where
  handleImages (Image attr ins (src, txt)) = Image attr ins (T.pack (toFilePath dir) <> "/" <> src, txt)
  handleImages x = x

-- | Flatten a pandoc `Meta` object to a `Value`.
flattenMeta :: MonadAction m => (Pandoc -> PandocIO Text) -> Meta -> m Value
flattenMeta opts meta = liftAction $ Slick.Pandoc.flattenMeta opts meta

{-|
  Get a JSON Value of Markdown Data with markdown body as "contents" field
  and the srcPath as "srcPath" field.
-}
loadMarkdownAsJSON :: (MonadAction m, MonadThrow m, FileLike Rel a)
                   => ReaderOptions
                   -> WriterOptions
                   -> a
                   -> m Value
loadMarkdownAsJSON ropts wopts srcPath = do
  pdoc@(Pandoc meta _) <- readMarkdownFile ropts srcPath
  meta' <- flattenMeta (writeHtml5String wopts) meta
  outText <- runPandocA $ writeHtml5String wopts pdoc
  return $ withStringField "src-path" (T.pack $ toFilePath $ toFile srcPath)
         $ withStringField "content" outText meta'
