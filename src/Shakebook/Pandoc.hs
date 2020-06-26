{-# LANGUAGE TemplateHaskell #-}

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
, viewContent
, viewSrcPath
, viewUrl
, loadMarkdownAsJSON
) where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.With
import           Development.Shake.Plus
import           Path.Extensions
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

-- | View the "content" field of a JSON value.
viewContent :: ToJSON a => a -> Text
viewContent = view' (key "content" . _String)

-- | Add "content" field from input Text.
withContent :: Text -> Value -> Value
withContent = withStringField "content"

-- | View the "src-path" field of a JSON Value.
viewSrcPath :: ToJSON a => a -> Text
viewSrcPath = view' (key "src-path" . _String)

-- | Add "src-path" field based on input Text.
withSrcPath :: Text -> Value -> Value
withSrcPath = withStringField "src-path"

-- | View the "url" field of a JSON Value.
viewUrl :: ToJSON a => a -> Text
viewUrl = view' (key "url" . _String)

-- | Add "url" field from input Text.
withUrl :: Text -> Value -> Value
withUrl = withStringField "url"

-- | Add a leading slash to a `Path Rel File` to turn it into a url as `Text`.
toGroundedUrl :: Path Rel File -> Text
toGroundedUrl = T.pack . toFilePath . ($(mkAbsDir "/") </>)

{-|
  Get a JSON Value of Markdown Data with markdown body as "contents" field
  and the srcPath as "srcPath" field.
-}
loadMarkdownAsJSON :: (MonadAction m, MonadThrow m)
                   => ReaderOptions
                   -> WriterOptions
                   -> Within Rel (Path Rel File)
                   -> m Value
loadMarkdownAsJSON ropts wopts srcPath = do
  pdoc@(Pandoc meta _) <- readMDFileWithin ropts srcPath
  meta' <- flattenMeta (writeHtml5String wopts) meta
  outText <- runPandocA $ writeHtml5String wopts pdoc
  supposedUrl <- toGroundedUrl <$> withHtmlExtension (extract srcPath)
  return $ withContent outText
         . withSrcPath (T.pack . toFilePath $ extract srcPath)
         . withUrl supposedUrl $ meta'

