{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Data where

import           Control.Comonad.Env as E
import           Control.Lens               hiding ((:<))
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Aeson.With
import           Development.Shake.Plus
import           Path.Extensions
import           RIO                        hiding (Lens', lens, view)
import qualified RIO.Text                   as T
import           Shakebook.Pandoc
import           Text.Pandoc

-- | View the "content" field of a JSON value.
viewContent :: Value -> Text
viewContent = view (key "content" . _String)

-- | Add "content" field from input Text.
withContent :: Text -> Value -> Value
withContent = withStringField "content"

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

-- | Add a leading slash to a `Path Rel File` to turn it into a url as `Text`.
toGroundedUrl :: Path Rel File -> Text
toGroundedUrl = T.pack . toFilePath . ($(mkAbsDir "/") </>)

-- | Generate a "supposed" url, the grounded version of the markdown source path.
generateSupposedUrl :: MonadThrow m => Path Rel File -> m Text
generateSupposedUrl srcPath = toGroundedUrl <$> withHtmlExtension srcPath

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
  supposedUrl <- generateSupposedUrl (extract srcPath)
  return $ withContent outText
         . withSrcPath (T.pack . toFilePath $ extract srcPath)
         . withUrl supposedUrl $ meta'
