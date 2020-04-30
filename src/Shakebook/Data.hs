module Shakebook.Data where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Lens               hiding ((:<))
import           Control.Monad.Extra
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Development.Shake          as S
import           Development.Shake.FilePath
import           RIO                        hiding (Lens', lens, view)
import           RIO.List
import           RIO.Partial
import qualified RIO.Text                   as T
import           Shakebook.Aeson
import           Slick
import           Slick.Pandoc
import           Text.Pandoc.Options

type ToC = Cofree [] String

data SbConfig = SbConfig
    { sbSrcDir  :: FilePath
    , sbOutDir  :: FilePath
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

type MonadShakebook r m = (MonadReader r m, HasSbConfig r, HasLogFunc r, MonadIO m)
type MonadShakebookAction r m = (MonadShakebook r m, MonadAction m)
type MonadShakebookRules r m = (MonadShakebook r m, MonadRules m)




-- View the "srcPath" field of a JSON Value.
viewSrcPath :: Value -> Text
viewSrcPath = view (key "srcPath" . _String)

-- Add "srcPath" field based on input Text.
withSrcPath :: Text -> Value -> Value
withSrcPath = withStringField "srcPath"

-- Add "baseUrl" field from input Text.
withBaseUrl :: Text -> Value -> Value
withBaseUrl = withStringField "baseUrl"

-- Add "fullUrl" field  from input Text.
withFullUrl :: Text -> Value -> Value
withFullUrl = withStringField "fullUrl"

-- View the "url" field of a JSON Value.
viewUrl :: Value -> Text
viewUrl = view (key "url" . _String)

-- Add "url" field from input Text.
withUrl :: Text -> Value -> Value
withUrl = withStringField "url"

-- Assuming a "url" field, enrich via a baseURL
enrichFullUrl :: Text -> Value -> Value
enrichFullUrl base v = withFullUrl (base <> viewUrl v) v

--- Assuming a 'srcPath' field, enrich using withUrl using a Text -> Text transformation.
enrichUrl :: (Text -> Text) -> Value -> Value
enrichUrl f v = withUrl (f (viewSrcPath v)) v

typicalFullOutToSrcPath :: MonadShakebook r m => m (String -> String)
typicalFullOutToSrcPath = view sbConfigL >>= \SbConfig{..} -> pure $
   drop 1 . fromJust . stripPrefix sbOutDir

typicalFullOutHTMLToMdSrcPath :: MonadShakebook r m => m (String -> String)
typicalFullOutHTMLToMdSrcPath = liftA2 (.) (pure (-<.> "md")) typicalFullOutToSrcPath

typicalMdSrcPathToHTMLFullOut :: MonadShakebook r m => m (String -> String)
typicalMdSrcPathToHTMLFullOut = view sbConfigL >>= \SbConfig{..} -> pure $
  (-<.> "html") . (sbOutDir </>) . drop 1 . fromJust . stripPrefix sbSrcDir

typicalSrcPathToUrl :: Text -> Text
typicalSrcPathToUrl = ("/" <>) . T.pack . (-<.> "html") . T.unpack

typicalUrlEnricher :: Value -> Value
typicalUrlEnricher v = withUrl (typicalSrcPathToUrl . viewSrcPath $ v) v

-- Get a JSON Value of Markdown Data with markdown body as "contents" field
-- and the srcPath as "srcPath" field.
readMarkdownFile' :: MonadShakebookAction r m => String -> m Value
readMarkdownFile' srcPath = view sbConfigL >>= \SbConfig{..} -> do
  logInfo $ displayShow $ "Reading source: " <> srcPath
  liftAction $ do
    docContent <- readFile' (sbSrcDir </> srcPath)
    docData <- markdownToHTMLWithOpts sbMdRead sbHTWrite . T.pack $ docContent
    return $ withSrcPath (T.pack srcPath) docData

loadIfExists :: (FilePath -> Action Value) -> FilePath -> Action Value
loadIfExists f src = ifM (S.doesFileExist src) (f src) (return (Object mempty))

getMarkdown :: MonadShakebookAction r m => [FilePattern] -> m [Value]
getMarkdown pat = view sbConfigL >>= \SbConfig{..} ->
  liftAction (getDirectoryFiles sbSrcDir pat) >>= mapM readMarkdownFile'

{--
getEnrichedMarkdown :: ReaderOptions -> WriterOptions -> (Value -> Value) -> FilePath -> [FilePattern] -> Action [Value]
getEnrichedMarkdown readOpts writeOpts f dir pat = fmap f <$> getDirectoryMarkdown readOpts writeOpts dir pat
--}
genBuildPageAction :: (MonadShakebookAction r m)
                   => FilePath -- The HTML template
                   -> (FilePath -> m Value) -- How to get an initial markdown JSON Object from the out filepath.
                   -> (Value -> Value) -- Additional modifiers for the value
                   -> FilePath -- The out filepath
                   -> m Value
genBuildPageAction template getData withData out = do
  logInfo $ displayShow $ "Generating page with fullpath " <> out
  pageT <- liftAction $ compileTemplate' template
  dataT <- withData . typicalUrlEnricher <$> getData out
  logDebug $ displayShow dataT
  writeFile' out . T.unpack $ substitute pageT dataT
  return dataT

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd f a = (a,) <$> f a

lower :: Cofree [] Value -> [Value]
lower (_ :< xs) = extract <$> xs

loadSortFilterEnrich :: (MonadShakebookAction r m, Ord b)
                              => [FilePattern] -- Filepattern
                              -> (Value -> b) -- A value to sortOn e.g (Down . viewPostTime)
                              -> (Value -> Bool) -- A filtering predicate e.g (elem tag . viewTags)
                              -> (Value -> Value) -- An initial enrichment. This is pure so can only be data derived from the initial markdown.
                              -> m [(String, Value)]
loadSortFilterEnrich pat s f e = view sbConfigL >>= \SbConfig {..} -> do
    allPosts <- liftAction $ getDirectoryFiles sbSrcDir $ map (-<.> ".md") pat
    readPosts <- sequence $ traverseToSnd readMarkdownFile' <$> allPosts
    return $ fmap (second e) $ sortOn (s . snd) $ filter (f . snd) readPosts

loadSortEnrich :: (MonadShakebookAction r m, Ord b) => [FilePattern] -> (Value -> b) -> (Value -> Value) -> m [(String, Value)]
loadSortEnrich pat s = loadSortFilterEnrich pat s (const True)
