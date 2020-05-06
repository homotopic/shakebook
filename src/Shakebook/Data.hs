{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Data where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Zipper.Extra
import           Control.Lens               hiding ((:<))
import           Control.Monad.Extra
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Development.Shake.Plus
import           Development.Shake (FilePattern)
import           Path                       as P
import           RIO                        hiding (Lens', lens, view)
import           RIO.List
import qualified RIO.Text                   as T
import           Shakebook.Aeson
import           Shakebook.Shake
import           Slick.Pandoc
import           Text.Pandoc.Options
import           Within

newtype PathDisplay a t = PathDisplay (Path a t)

instance Display (PathDisplay a t) where
  display (PathDisplay f) = displayBytesUtf8 . fromString . toFilePath $ f

newtype WithinDisplay a t = WithinDisplay (Within a t)

instance Display (WithinDisplay a t) where
  display (WithinDisplay (Within (x,y))) = display (PathDisplay x) <> "[" <> display (PathDisplay y) <> "]"

instance Display [WithinDisplay a t] where
  display [] = ""
  display (x : xs) = display x <> " : " <> display xs

type ToC = Cofree [] String

data SbConfig = SbConfig
    { sbSrcDir  :: Path Rel Dir
    , sbOutDir  :: Path Rel Dir
    , sbBaseUrl :: Text
    , sbMdRead  :: ReaderOptions
    , sbHTWrite :: WriterOptions
    , sbPPP     :: Int
    }
    deriving (Show)

class HasSbConfig a where
  sbConfigL :: Lens' a SbConfig

data ShakebookEnv = ShakebookEnv
    { logFunc  :: LogFunc
    , sbConfig :: SbConfig
    }

instance HasLocalOut ShakebookEnv where
  localOutL = lens (sbOutDir . sbConfig) undefined

instance HasSbConfig ShakebookEnv where
  sbConfigL = lens sbConfig undefined

instance HasLogFunc ShakebookEnv where
  logFuncL = lens logFunc undefined

-- | View the "srcPath" field of a JSON Value.
viewSrcPath :: Value -> Text
viewSrcPath = view (key "srcPath" . _String)

-- | Add "srcPath" field based on input Text.
withSrcPath :: Text -> Value -> Value
withSrcPath = withStringField "srcPath"

-- | Add "baseUrl" field from input Text.
withBaseUrl :: Text -> Value -> Value
withBaseUrl = withStringField "baseUrl"

-- | Add "fullUrl" field  from input Text.
withFullUrl :: Text -> Value -> Value
withFullUrl = withStringField "fullUrl"

-- | View the "url" field of a JSON Value.
viewUrl :: Value -> Text
viewUrl = view (key "url" . _String)

-- | Add "url" field from input Text.
withUrl :: Text -> Value -> Value
withUrl = withStringField "url"

-- | Assuming a "url" field, enrich via a baseURL
enrichFullUrl :: Text -> Value -> Value
enrichFullUrl base v = withFullUrl (base <> viewUrl v) v

-- | Assuming a 'srcPath' field, enrich using withUrl using a Text -> Text transformation.
enrichUrl :: (Text -> Text) -> Value -> Value
enrichUrl f v = withUrl (f (viewSrcPath v)) v

leadingSlash :: Path Abs Dir
leadingSlash = $(mkAbsDir "/")

withHtmlExtension :: MonadThrow m => Path Rel File -> m (Path Rel File)
withHtmlExtension = replaceExtension ".html"

withMarkdownExtension :: MonadThrow m => Path Rel File -> m (Path Rel File)
withMarkdownExtension = replaceExtension ".md"

generateSupposedUrl :: MonadThrow m => Path Rel File -> m (Path Abs File)
generateSupposedUrl srcPath = (leadingSlash </>) <$> withHtmlExtension srcPath

enrichSupposedUrl :: (MonadReader r m, HasSbConfig r, MonadThrow m) => Value -> m Value
enrichSupposedUrl v = view sbConfigL >>= \SbConfig{..} -> do
  x <- parseRelFile $ T.unpack $ viewSrcPath v
  y <- generateSupposedUrl x
  return $ withUrl (T.pack . toFilePath $ y) v

{-|
  Get a JSON Value of Markdown Data with markdown body as "contents" field
  and the srcPath as "srcPath" field.
-}
readMarkdownFile' :: (MonadReader r m, HasSbConfig r, MonadAction m)
                  => Within Rel File
                  -> m Value
readMarkdownFile' srcPath = view sbConfigL >>= \SbConfig{..} -> liftAction $ do
  docContent <- readFile' (fromWithin srcPath)
  docData <- markdownToHTMLWithOpts sbMdRead sbHTWrite docContent
  supposedUrl <- liftIO $ (leadingSlash </>) <$> withHtmlExtension (whatLiesWithin srcPath)
  return $ withSrcPath (T.pack . toFilePath $ whatLiesWithin srcPath)
         . withUrl (T.pack . toFilePath $ supposedUrl) $ docData

data PaginationException = EmptyContentsError
  deriving (Show, Eq, Typeable)

instance Exception PaginationException where
  displayException EmptyContentsError = "Can not create a Zipper of length zero."

paginate' :: MonadThrow m => Int -> [a] -> m (Zipper [] [a])
paginate' n xs =  case paginate n xs of
                    Just x -> return x
                    Nothing -> throwM EmptyContentsError

lower :: Cofree [] Value -> [Value]
lower (_ :< xs) = extract <$> xs

type MonadShakebook r m = (MonadReader r m, HasSbConfig r, HasLogFunc r, MonadIO m, MonadThrow m, HasLocalOut r)
type MonadShakebookAction r m = (MonadShakebook r m, MonadAction m)
type MonadShakebookRules r m = (MonadShakebook r m, MonadRules m)


{-|
  Multi-markdown loader. Allows you to load a filepattern of markdown as a list of JSON
  values ready to pass to an HTML template. You will probably want to add additional
  data before you write. See the examples in Shakebook.Defaults
-}
loadSortFilterEnrich :: (MonadShakebookAction r m, Ord b)
                     => [FilePattern]    -- ^ A shake filepattern to load, relative to srcDir from SbConfig.
                     -> (Value -> b)     -- ^ A value to sortOn e.g (Down . viewPostTime)
                     -> (Value -> Bool)  -- ^ A filtering predicate e.g (elem tag . viewTags)
                     -> (Value -> Value) -- ^ An initial enrichment. This is pure so can only be data derived from the initial markdown.
                     -> m [(Within Rel File, Value)] -- ^ A list of Values indexed by their srcPath.
loadSortFilterEnrich pat s f e = view sbConfigL >>= \SbConfig {..} -> do
    allPosts <- liftAction $ getDirectoryFilesWithin' sbSrcDir pat
    readPosts <- sequence $ traverseToSnd readMarkdownFile' <$> allPosts
    return $ fmap (second e) $ sortOn (s . snd) $ filter (f . snd) readPosts

-- | The same as `loadSortFilterEnrich` but without filtering.
loadSortEnrich :: (MonadShakebookAction r m, Ord b)
               => [FilePattern]    -- ^ A Shake filepattern to load.
               -> (Value -> b)     -- ^ A value to sortOn e.g (Down . viewPostTime).
               -> (Value -> Value) -- ^ An initial pure enrichment.
               -> m [(Within Rel File, Value)] -- ^ A list of Values index by their srcPath.
loadSortEnrich pat s = loadSortFilterEnrich pat s (const True)
