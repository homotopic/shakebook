module Shakebook.Data where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Lens                 hiding ((:<))
import           Control.Monad.Extra
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake as S
import           Development.Shake.FilePath
import           RIO                          hiding (view)
import           RIO.Partial
import qualified RIO.HashMap                  as HML
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Text                     as T
import qualified RIO.Text.Lazy                as TL
import qualified RIO.Text.Partial             as T
import           RIO.Time
import qualified RIO.Vector                   as V
import           Slick
import           Slick.Pandoc
import           Text.Atom.Feed               as Atom
import           Text.Atom.Feed.Export        as Atom
import           Text.Pandoc.Highlighting
import           Shakebook.Conventions
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

traverseToSnd f a = (a,) <$> f a

lower :: Cofree [] Value -> [Value]
lower (_ :< xs) = extract <$> xs
