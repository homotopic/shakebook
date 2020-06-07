module Shakebook.Pandoc (
  runPandocA
, PandocActionException(..)
, readMDFile
, readMDFileIn
, readMDFileWithin
, needPandocImagesIn
, makePDFLaTeX
, progressivelyDemoteHeaders
, replaceUnusableImages
, prefixAllImages
, flattenMeta
) where

import Control.Comonad.Cofree
import Data.Aeson
import Development.Shake.Plus
import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as T
import qualified Slick.Pandoc
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Readers
import Text.Pandoc.Options
import Text.Pandoc.PDF
import Text.Pandoc.Templates
import Text.Pandoc.Walk
import Text.Pandoc.Writers

data PandocActionException = PandocActionException String
  deriving (Show, Eq, Typeable)

instance Exception PandocActionException where
  displayException (PandocActionException s) = s

-- | Natural transformation from `PandocIO` to a `MonadAction`
runPandocA :: (MonadAction m, MonadThrow m ) => PandocIO a -> m a
runPandocA p = do
  result <- liftIO $ runIO p
  either throwM return result

-- | Read a markdown file as an Action.
readMDFile :: (MonadAction m, MonadThrow m) => ReaderOptions -> Path Rel File -> m Pandoc
readMDFile ropts src = readFile' src >>= runPandocA . readMarkdown ropts

-- | Read a markdown file as an Action.
readMDFileIn :: (MonadAction m, MonadThrow m) => ReaderOptions -> Path Rel Dir -> Path Rel File -> m Pandoc
readMDFileIn ropts dir src = readFile' (dir </> src) >>= runPandocA . readMarkdown ropts

-- | Like `readMDFile` but accepts a `Within`
readMDFileWithin :: (MonadAction m, MonadThrow m) => ReaderOptions -> Within Rel (Path Rel File) -> m Pandoc
readMDFileWithin ropts src = readMDFile ropts (fromWithin src)

-- | Find all the images in a `Pandoc` data structure and call `Development.Shake.Plus.need` on them.
needPandocImagesIn :: (MonadAction m, MonadThrow m) => Path Rel Dir -> Pandoc -> m ()
needPandocImagesIn outDir pdoc =
  mapM parseRelFile (fmap (drop 1 . T.unpack) $ pullImages pdoc) >>= needIn outDir where
    pullImages = query f
    f (Image _ _ (src, _)) = [src]
    f _ = []

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
progressivelyDemoteHeaders = pushHeaders (0) where
  handleHeaders :: Int -> Block -> Block
  handleHeaders i (Header a as xs) = Header (max 1 (a + i)) as xs
  handleHeaders _ x                = x

  pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
  pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

-- | For a list of file extensions, replace the images with an Inline based on its src path.
replaceUnusableImages :: MonadThrow m => [String] -> (Text -> Inline) -> Pandoc -> m (Pandoc)
replaceUnusableImages exts f = walkM handleImages where
  handleImages i@(Image _ _ (src, _)) = do
    x <- parseAbsFile (T.unpack src) >>= fileExtension
    return $ if elem x exts then f src else i
  handleImages x = return x

prefixAllImages :: Path Rel Dir -> Pandoc -> Pandoc
prefixAllImages dir = walk handleImages where
  handleImages (Image attr ins (src, txt)) = Image attr ins ((T.pack $ toFilePath dir) <> "/" <> src, txt)
  handleImages x = x

flattenMeta :: MonadAction m => (Pandoc -> PandocIO Text) -> Meta -> m Value
flattenMeta opts meta = liftAction $ Slick.Pandoc.flattenMeta opts meta
