{- |
   Module     : Shakebook.Pandoc
   License    : MIT
   Stability  : experimental

Pandoc utilities lifted to `MonadAction`.
-}
{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Pandoc where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Data.Aeson
import           Data.Aeson.With
import           Development.Shake.Plus hiding ((:->))
import           RIO
import qualified RIO.ByteString.Lazy    as LBS
import qualified RIO.Text               as T
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.PDF
import           Text.Pandoc.Readers
import           Text.Pandoc.Templates
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers
import Composite.Aeson
import Composite.Record
import Text.Pandoc.Throw
import Data.Vinyl.TypeLevel
import Shakebook.Aeson
import Data.Vinyl hiding (RElem)
import Composite.TH
import Composite.Aeson.Throw

newtype PandocActionException = PandocActionException String
    deriving (Show, Eq, Typeable)

instance Exception PandocActionException where
  displayException (PandocActionException s) = s

-- | Natural transformation from `PandocIO` to a `MonadAction`
runPandocA :: (MonadAction m, MonadThrow m) => PandocIO a -> m a
runPandocA p = do
  result <- liftIO $ runIO p
  either throwM return result

-- | Run a Pandoc reader as a Shake action.
readFilePandoc :: (MonadAction m, MonadThrow m) => (ReaderOptions -> Text -> PandocIO Pandoc) -> ReaderOptions -> Path b File -> m Pandoc
readFilePandoc run ropts src = readFile' src >>= runPandocA . run ropts

-- | Read a markdown file and return a `Pandoc` as an Action.
readMarkdownFile :: (MonadAction m, MonadThrow m) => ReaderOptions -> Path b File -> m Pandoc
readMarkdownFile = readFilePandoc readMarkdown

-- | Read a mediawiki file and return a `Pandoc` as an Action.
readMediaWikiFile :: (MonadAction m, MonadThrow m) => ReaderOptions -> Path b File -> m Pandoc
readMediaWikiFile = readFilePandoc readMediaWiki

-- | Read a LaTeX file and return a `Pandoc` as an Action.
readLaTeXFile :: (MonadAction m, MonadThrow m) => ReaderOptions -> Path b File -> m Pandoc
readLaTeXFile = readFilePandoc readLaTeX

-- | Read a CSV file and return a `Pandoc` as an Action.
readCSVFile :: (MonadAction m, MonadThrow m) => ReaderOptions -> Path b File -> m Pandoc
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


withLensesAndProxies [d|
  type FContent = "content" :-> Text
  |]

writeBlocksDefault :: WriterOptions -> [Block] -> Text
writeBlocksDefault wopts x = runPandocPureDefault "" (writeHtml5String wopts $ Pandoc mempty x)

runPandocPureDefault :: a -> PandocPure a -> a
runPandocPureDefault x = either (const x) id . runPure

{-|
  Get a JSON Value of Markdown Data with markdown body as "contents" field
  and the srcPath as "srcPath" field.
-}
loadMarkdownAsJSON :: (MonadIO m, MonadThrow m, Show e, Typeable e)
                   => ReaderOptions
                   -> WriterOptions
                   -> JsonFormat e (Record a)
                   -> Path b File
                   -> m (Record (Compdoc a))
loadMarkdownAsJSON ropts wopts f srcPath = do
  k <- readFileUtf8 (toFilePath srcPath)
  readMarkdown' ropts wopts f k

-- | A Compdoc is a Record with at least an FContent field.
type Compdoc a = a ++ (FContent : '[])

-- | Read some Text as pandoc markdown and
readMarkdown' :: (Show e, Typeable e, MonadThrow m) => ReaderOptions -> WriterOptions -> JsonFormat e (Record a) -> T.Text -> m (Record (Compdoc a))
readMarkdown' ropts wopts f x = runPandocPureThrow (Text.Pandoc.Readers.readMarkdown ropts x) >>= pandocToCompdoc writeHtml5String wopts f

pandocToCompdoc :: (Typeable e, Show e, MonadThrow m) => (WriterOptions -> Pandoc -> PandocPure T.Text) -> WriterOptions -> JsonFormat e (Record a) -> Pandoc -> m (Record (Compdoc a))
pandocToCompdoc writer wopts f (Pandoc meta xs) = flattenMeta (writer wopts) meta >>= parseValue' f >>= return . (<+> contentBlock wopts xs)

contentBlock :: WriterOptions -> [Block] -> Record (FContent : '[])
contentBlock wopts x = writeBlocksDefault wopts x :*: RNil

flattenMeta :: MonadThrow m => (Pandoc -> PandocPure T.Text) -> Meta -> m Value
flattenMeta writer (Meta meta) = toJSON <$> traverse go meta
 where
  go :: MonadThrow m => MetaValue -> m Value
  go (MetaMap     m) = toJSON <$> traverse go m
  go (MetaList    m) = toJSONList <$> traverse go m
  go (MetaBool    m) = pure $ toJSON m
  go (MetaString  m) = pure $ toJSON m
  go (MetaInlines m) = toJSON <$> (runPandocPureThrow . writer . Pandoc mempty . (:[]) . Plain $ m)
  go (MetaBlocks  m) = toJSON <$> (runPandocPureThrow . writer . Pandoc mempty $ m)

