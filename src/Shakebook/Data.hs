module Shakebook.Data where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Monad.Extra
import           Data.Aeson                   as A
import           Development.Shake as S
import           Development.Shake.FilePath
import           RIO                          hiding (view)
import           RIO.List
import qualified RIO.Text                     as T
import           Slick
import           Slick.Pandoc
import           Shakebook.Conventions
import           Shakebook.Zipper
import           Text.Pandoc.Options

type ToC = Cofree [] String

-- Get a JSON Value of Markdown Data with markdown body as "contents" field
-- and the srcPath as "srcPath" field.
readMarkdownFile' :: ReaderOptions -> WriterOptions -> String -> Action Value
readMarkdownFile' readerOptions writerOptions srcPath = do
  docContent <- readFile' srcPath
  docData <- markdownToHTMLWithOpts readerOptions writerOptions . T.pack $ docContent
  return $ withSrcPath (T.pack srcPath) docData

loadIfExists :: (FilePath -> Action Value) -> FilePath -> Action Value
loadIfExists f src = ifM (S.doesFileExist src) (f src) (return (Object mempty))

getDirectoryMarkdown :: ReaderOptions -> WriterOptions -> FilePath -> [FilePattern] -> Action [Value]
getDirectoryMarkdown readOpts writeOpts dir pat = do
  getDirectoryFiles dir pat >>= mapM (readMarkdownFile' readOpts writeOpts . (dir </>))

getEnrichedMarkdown :: ReaderOptions -> WriterOptions -> (Value -> Value) -> FilePath -> [FilePattern] -> Action [Value]
getEnrichedMarkdown readOpts writeOpts f dir pat = fmap f <$> getDirectoryMarkdown readOpts writeOpts dir pat

genBuildPageAction :: FilePath -- The HTML template
                   -> (FilePath -> Action Value) -- How to get an initial markdown JSON Object from the out filepath.
                   -> (Value -> Value) -- Additional modifiers for the value
                   -> FilePath -- The out filepath 
                   -> Action Value
genBuildPageAction template getData withData out = do
  pageT <- compileTemplate' template
  dataT <- withData <$> getData out
  writeFile' out . T.unpack $ substitute pageT dataT
  return dataT

genIndexPageData :: [Value]
                 -> Text
                 -> (Text -> Text)
                 -> Int
                 -> Maybe (Zipper [] Value)
genIndexPageData xs g h n = fmap (extend (genPageData g h)) $ paginate n xs

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd f a = (a,) <$> f a

lower :: Cofree [] Value -> [Value]
lower (_ :< xs) = extract <$> xs

data SbConfig = SbConfig {
   sbSrcDir  :: FilePath
,  sbOutDir  :: FilePath
,  sbMdRead  :: ReaderOptions
,  sbHTWrite :: WriterOptions
,  sbPPP :: Int
} deriving (Show)

newtype Shakebook a = Shakebook ( ReaderT SbConfig Rules a )
  deriving (Functor, Applicative, Monad, MonadReader SbConfig)

newtype ShakebookA a = ShakebookA ( ReaderT SbConfig Action a )
  deriving (Functor, Applicative, Monad, MonadReader SbConfig)

runShakebook :: SbConfig -> Shakebook a -> Rules a
runShakebook c (Shakebook f) = runReaderT f c

runShakebookA :: SbConfig -> ShakebookA a -> Action a
runShakebookA c (ShakebookA f) = runReaderT f c


loadSortFilterEnrich :: Ord b => [FilePattern] -- Filepattern
                              -> (Value -> b) -- A value to sortOn e.g (Down . viewPostTime)
                              -> (Value -> Bool) -- A filtering predicate e.g (elem tag . viewTags)
                              -> (Value -> Value) -- An initial enrichment. This is pure so can only be data derived from the initial markdown.
                              -> ShakebookA [(String, Value)]
loadSortFilterEnrich pat s f e = ShakebookA $ ask >>= \SbConfig {..} -> lift $ do
  allPosts <- getDirectoryFiles sbSrcDir $ map (-<.> ".md") pat
  readPosts <- sequence $ traverseToSnd (readMarkdownFile' sbMdRead sbHTWrite . (sbSrcDir </>)) <$> allPosts
  return $ fmap (second e) $ sortOn (s . snd) $ filter (f . snd) $ readPosts

loadSortEnrich :: Ord b => [FilePattern] -> (Value -> b) -> (Value -> Value) -> ShakebookA [(String, Value)]
loadSortEnrich pat s e = loadSortFilterEnrich pat s (const True) e
