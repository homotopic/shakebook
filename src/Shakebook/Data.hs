{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Data where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Lens               hiding ((:<))
import           Control.Monad.Extra
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Path                       as P
import           RIO                        hiding (Lens', lens, view)
import           RIO.List
import qualified RIO.Text                   as T
import           Shakebook.Aeson
import           Shakebook.Zipper
import           Shakebook.Shake
import           Shakebook.Within
import           Shakebook.Mustache
import           Slick hiding (compileTemplate')
import           Slick.Pandoc
import           Text.Pandoc.Options

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

newtype Shakebook r a = Shakebook ( ReaderT r Rules a )
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO)

instance MonadThrow (Shakebook r) where
  throwM = throwIO

instance MonadThrow (ShakebookA r) where
  throwM = throwIO

newtype ShakebookA r a = ShakebookA ( ReaderT r Action a )
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO)

runShakebook :: r -> Shakebook r a -> Rules a
runShakebook c (Shakebook f) = runReaderT f c

runShakebookA :: r -> ShakebookA r a -> Action a
runShakebookA c (ShakebookA f) = runReaderT f c

class MonadAction m where
  liftAction :: Action a -> m a

class MonadRules m where
  liftRules :: Rules a -> m a

instance MonadAction (ShakebookA r) where
  liftAction = ShakebookA . lift

instance MonadRules (Shakebook r) where
  liftRules = Shakebook . lift

data ShakebookEnv = ShakebookEnv
    { logFunc  :: LogFunc
    , sbConfig :: SbConfig
    }

instance HasSbConfig ShakebookEnv where
  sbConfigL = lens sbConfig undefined

instance HasLogFunc ShakebookEnv where
  logFuncL = lens logFunc undefined

type MonadShakebook r m = (MonadReader r m, HasSbConfig r, HasLogFunc r, MonadIO m, MonadThrow m)
type MonadShakebookAction r m = (MonadShakebook r m, MonadAction m)
type MonadShakebookRules r m = (MonadShakebook r m, MonadRules m)




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

-- | Filepath/URL calculators - these work but don't try to do the wrong thing or it will explode.
stripSbOutDir :: (MonadThrow m, MonadReader r m, HasSbConfig r) => Path Rel t -> m (Path Rel t)
stripSbOutDir x = view sbConfigL >>= \SbConfig{..} -> stripProperPrefix sbOutDir x

stripSbSrcDir :: (MonadThrow m, MonadReader r m, HasSbConfig r) => Path Rel t -> m (Path Rel t)
stripSbSrcDir x = view sbConfigL >>= \SbConfig{..} -> stripProperPrefix sbSrcDir x

inSbSrcDir :: MonadShakebook r m => Path Rel t -> m (Path Rel t)
inSbSrcDir x = view sbConfigL >>= \SbConfig{..} -> return $ (sbSrcDir </>) x

inSbOutDir :: MonadShakebook r m => Path Rel t -> m (Path Rel t)
inSbOutDir x = view sbConfigL >>= \SbConfig{..} -> return $ (sbOutDir </>) x

leadingSlash :: Path Abs Dir
leadingSlash = $(mkAbsDir "/")

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
  return $ withSrcPath (T.pack . toFilePath $ whatLiesWithin srcPath) docData

data PaginationException = EmptyContentsError
  deriving (Show, Eq, Typeable)

instance Exception PaginationException where
  displayException (EmptyContentsError) = "Can not create a Zipper of length zero."

paginate' :: MonadShakebookAction r m => [a] -> m (Zipper [] [a])
paginate' xs = view sbConfigL >>= \SbConfig{..} -> do
                 case paginate sbPPP xs of
                    Just x -> return x
                    Nothing -> throwM EmptyContentsError
  
getMarkdown :: MonadShakebookAction r m => [FilePattern] -> m [Value]
getMarkdown pat = view sbConfigL >>= \SbConfig{..} ->
  liftAction (getDirectoryFilesWithin' sbSrcDir pat) >>= mapM readMarkdownFile'

{-| 
  Build a single page straight from a template, a loaded Value, and a pure enrichment.
-}
genBuildPageAction :: Path Rel File -- ^ The HTML template
                   -> (Path Rel File -> Action Value) -- ^ How to get from FilePath to Value.
                   -> (Value -> Value) -- ^ Additional modifiers for the value.
                   -> Path Rel File -- ^ The out filepath
                   -> Action Value
genBuildPageAction template getData withData out = do
  pageT <- compileTemplate' template
  dataT <- withData <$> getData out
  writeFile' out $ substitute pageT dataT
  return dataT

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd f a = (a,) <$> f a

lower :: Cofree [] Value -> [Value]
lower (_ :< xs) = extract <$> xs

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
