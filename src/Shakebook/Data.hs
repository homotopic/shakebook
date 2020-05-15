{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Data where

import           Control.Comonad.Env as E
import           Control.Comonad.Cofree
import           Control.Lens               hiding ((:<))
import           Control.Monad.Extra
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Aeson.With
import           Development.Shake.Plus
import           Path                       as P
import           RIO                        hiding (Lens', lens, view)
import qualified RIO.Text                   as T
import           Slick.Pandoc
import           Text.Pandoc
import           Text.Pandoc.Walk
import           Within

newtype PathDisplay a t = PathDisplay (Path a t)

instance Display (PathDisplay a t) where
  display (PathDisplay f) = displayBytesUtf8 . fromString . toFilePath $ f

newtype WithinDisplay a t = WithinDisplay (Within a t)

instance Display t => Display (WithinDisplay a t) where
  display (WithinDisplay (WithinT (EnvT e (Identity a)))) = display (PathDisplay e) <> "[" <> display a <> "]"

instance Display t => Display [WithinDisplay a t] where
  display [] = ""
  display (x : xs) = display x <> " : " <> display xs

data SbConfig = SbConfig
    { sbSrcDir      :: Path Rel Dir
    , sbOutDir      :: Path Rel Dir
    , sbBaseUrl     :: Text
    , sbMdRead      :: ReaderOptions
    , sbHTWrite     :: WriterOptions
    , sbPPP         :: Int
    , sbGlobalApply :: Value -> Value
    }

data ShakebookEnv = ShakebookEnv
    { logFunc  :: LogFunc
    , sbConfig :: SbConfig
    }

class HasSbConfig a where
  sbConfigL :: Lens' a SbConfig

instance HasSbConfig ShakebookEnv where
  sbConfigL = lens sbConfig undefined

instance HasLogFunc ShakebookEnv where
  logFuncL = lens logFunc undefined

-- | View the "src-path" field of a JSON Value.
viewSrcPath :: Value -> Text
viewSrcPath = view (key "src-path" . _String)

-- | Add "src-path" field based on input Text.
withSrcPath :: Text -> Value -> Value
withSrcPath = withStringField "src-path"

-- | View the "base-url" of a JSON Value.
viewBaseUrl :: Value -> Text
viewBaseUrl = view (key "base-url" . _String)

-- | Add "base-url" field from input Text.
withBaseUrl :: Text -> Value -> Value
withBaseUrl = withStringField "base-url"

-- | View the "full-url" of a JSON Value.
viewFullUrl :: Value -> Text
viewFullUrl = view (key "full-url" . _String)

-- | Add "full-url" field  from input Text.
withFullUrl :: Text -> Value -> Value
withFullUrl = withStringField "full-url"

-- | View the "image" field of a JSON vaule.
viewImage :: Value -> Text
viewImage = view (key "image" . _String)

-- | View the "url" field of a JSON Value.
viewUrl :: Value -> Text
viewUrl = view (key "url" . _String)

-- | Add "url" field from input Text.
withUrl :: Text -> Value -> Value
withUrl = withStringField "url"

-- | Assuming a "url" field, enrich via a baseURL
enrichFullUrl :: Text -> Value -> Value
enrichFullUrl base v = withFullUrl (base <> viewUrl v) v

-- | Assuming a 'src-path' field, enrich using withUrl using a Text -> Text transformation.
enrichUrl :: (Text -> Text) -> Value -> Value
enrichUrl f v = withUrl (f (viewSrcPath v)) v

leadingSlash :: Path Abs Dir
leadingSlash = $(mkAbsDir "/")

withHtmlExtension :: MonadThrow m => Path Rel File -> m (Path Rel File)
withHtmlExtension = replaceExtension ".html"

withMarkdownExtension :: MonadThrow m => Path Rel File -> m (Path Rel File)
withMarkdownExtension = replaceExtension ".md"

withHaskellExtension :: MonadThrow m => Path Rel File -> m (Path Rel File)
withHaskellExtension = replaceExtension ".hs"

generateSupposedUrl :: MonadThrow m => Path Rel File -> m (Path Abs File)
generateSupposedUrl srcPath = (leadingSlash </>) <$> withHtmlExtension srcPath

enrichSupposedUrl :: (MonadReader r m, HasSbConfig r, MonadThrow m) => Value -> m Value
enrichSupposedUrl v = view sbConfigL >>= \SbConfig{..} -> do
  x <- parseRelFile $ T.unpack $ viewSrcPath v
  y <- generateSupposedUrl x
  return $ withUrl (T.pack . toFilePath $ y) v

unPandocM :: (MonadAction m, MonadFail m ) => PandocIO a -> m a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) return result

getImages :: Pandoc -> [Text]
getImages = query f where
  f (Image _ _ (src, _)) = [src]
  f _ = []

{-|
  Get a JSON Value of Markdown Data with markdown body as "contents" field
  and the srcPath as "srcPath" field.
-}
readMarkdownFile' :: (MonadReader r m, HasSbConfig r, MonadAction m, MonadFail m, MonadThrow m)
                  => Within Rel (Path Rel File)
                  -> m Value
readMarkdownFile' srcPath = view sbConfigL >>= \SbConfig{..} -> do
  docContent <- readFile' $ liftA2 (</>) E.ask extract $ srcPath
  pdoc@(Pandoc meta _) <- unPandocM $ readMarkdown sbMdRead docContent
  meta' <- liftAction $ flattenMeta (writeHtml5String sbHTWrite) meta
  let images = getImages pdoc
  mapM parseRelFile (fmap (drop 1 . T.unpack) images) >>= needIn sbOutDir 
  outText <- unPandocM $ writeHtml5String sbHTWrite pdoc
  let docData = meta' & _Object . at "content" ?~ String outText
  supposedUrl <- liftIO $ (leadingSlash </>) <$> withHtmlExtension (extract srcPath)
  return $ sbGlobalApply
         . withSrcPath (T.pack . toFilePath $ extract srcPath)
         . withUrl (T.pack . toFilePath $ supposedUrl) $ docData

immediateShoots :: Cofree [] a -> [a]
immediateShoots(_ :< xs) = extract <$> xs

type MonadShakebook r m = (MonadReader r m, HasSbConfig r, HasLogFunc r, MonadIO m, MonadThrow m)
type MonadShakebookAction r m = (MonadShakebook r m, MonadAction m)
type MonadShakebookRules r m = (MonadShakebook r m, MonadRules m)
