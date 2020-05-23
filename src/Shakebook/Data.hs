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
import           Shakebook.Pandoc
import           Text.Pandoc
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

-- | Add "content" field from input Text.
withContent :: Text -> Value -> Value
withContent = withStringField "content"

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

enrichSupposedUrl :: MonadThrow m => Value -> m Value
enrichSupposedUrl v = do
  x <- parseRelFile $ T.unpack $ viewSrcPath v
  y <- generateSupposedUrl x
  return $ withUrl (T.pack . toFilePath $ y) v

{-|
  Get a JSON Value of Markdown Data with markdown body as "contents" field
  and the srcPath as "srcPath" field.
-}
loadMarkdownAsJSON :: (MonadReader r m, HasSbConfig r, MonadAction m, MonadThrow m)
                  => Within Rel (Path Rel File)
                  -> m Value
loadMarkdownAsJSON srcPath = view sbConfigL >>= \SbConfig{..} -> do
  pdoc@(Pandoc meta _) <- readMDFileWithin sbMdRead srcPath
  meta' <- flattenMeta (writeHtml5String sbHTWrite) meta
  needPandocImagesIn sbOutDir pdoc
  outText <- runPandocA $ writeHtml5String sbHTWrite pdoc
  supposedUrl <- generateSupposedUrl (extract srcPath)
  return $ sbGlobalApply
         . withContent outText
         . withSrcPath (T.pack . toFilePath $ extract srcPath)
         . withUrl (T.pack . toFilePath $ supposedUrl) $ meta'

immediateShoots :: Cofree [] a -> [a]
immediateShoots(_ :< xs) = extract <$> xs
