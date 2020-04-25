{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shakebook.Defaults where
                                                                                                        

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Store.Class
import           Control.Comonad.Store.Zipper
import           Control.Monad.Extra
import           Data.Aeson                 as A
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake          as S
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           RIO                        hiding (view)
import qualified RIO.ByteString.Lazy        as LBS
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Map                    as M
import           RIO.Partial
import qualified RIO.Text                   as T
import           RIO.Time
import           Shakebook.Data
import           Shakebook.Rules
import           Text.DocTemplates
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.PDF
import           Text.Pandoc.Readers
import           Text.Pandoc.Templates
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers

monthURLFormat :: UTCTime -> String
monthURLFormat = formatTime defaultTimeLocale "%Y-%m"

prettyMonthFormat :: UTCTime -> String
prettyMonthFormat = formatTime defaultTimeLocale "%B, %Y"

prettyTimeFormat :: UTCTime -> String
prettyTimeFormat = formatTime defaultTimeLocale "%A, %B %d, %Y"

monthIndexUrlFormat :: UTCTime -> String
monthIndexUrlFormat t = "/posts/months" </> monthURLFormat t

enrichPost :: Value -> Value
enrichPost = enrichTeaser "<!--more-->"
           . enrichTagLinks ("/posts/tags/" <>)
           . enrichPrettyDate prettyTimeFormat
           . enrichTypicalUrl

--Data models-------------------------------------------------------------------

sortAndFilterValues :: Ord a => [FilePath] -> (Value -> a) -> (Value -> Bool) -> Action [(FilePath, Value)]
sortAndFilterValues xs s f = do
  ys <- forM xs $ \x -> do
    k <- defaultReadMarkdownFile x
    return (x, k)
  return $ sortOn (s . snd) $ filter (f . snd) ys


defaultReadMarkdownFile :: FilePath -> Action Value
defaultReadMarkdownFile = readMarkdownFile' markdownReaderOptions html5WriterOptions

defaultGetDirectoryMarkdown :: FilePath -> [FilePattern] -> Action [Value]  
defaultGetDirectoryMarkdown = getDirectoryMarkdown markdownReaderOptions html5WriterOptions

getEnrichedPostData :: FilePath -> [FilePattern] -> Action [Value]
getEnrichedPostData = getEnrichedMarkdown markdownReaderOptions html5WriterOptions enrichPost 

markdownReaderOptions :: ReaderOptions
markdownReaderOptions = def { readerExtensions = pandocExtensions }

html5WriterOptions :: WriterOptions
html5WriterOptions = def { writerHTMLMathMethod = MathJax ""}

latexWriterOptions :: WriterOptions
latexWriterOptions = def { writerTableOfContents = True
                         , writerVariables = Context $ M.fromList [
                                               ("geometry", SimpleVal "margin=3cm")
                                             , ("fontsize", SimpleVal "10")
                                             , ("linkcolor",SimpleVal "blue")]
                         }

makePDFLaTeX :: Pandoc -> PandocIO (Either LBS.ByteString LBS.ByteString)
makePDFLaTeX p = do
  t <- compileDefaultTemplate "latex"
  makePDF "pdflatex" [] writeLaTeX latexWriterOptions { writerTemplate = Just t } p

handleImages :: (Text -> Text) -> Inline -> Inline
handleImages f (Image attr ins (src,txt)) =
  if T.takeEnd 4 src == ".mp4" then Str (f src)
  else Image attr ins ("public/" <> src, txt)
handleImages _ x = x

handleHeaders :: Int -> Block -> Block
handleHeaders i (Header a as xs) = Header (max 1 (a + i)) as xs
handleHeaders _ x                  = x

pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

defaultDocsPatterns :: Cofree [] FilePath -- Rosetree Table of Contents.
                    -> FilePath
                    -> (Value -> Value) -- Extra data modifiers.
                    -> Shakebook ()
defaultDocsPatterns toc template withData = Shakebook $ ask >>=
  \ShakebookConfig {sbSrcDir, sbOutDir, sbMdRead, sbHTWrite} -> do
    let r = readMarkdownFile' sbMdRead sbHTWrite
    lift $ cofreeRuleGen toc ((sbOutDir </>) . (-<.> ".html")) (
           \xs -> \out -> do 
               ys <- mapM r (fmap (sbSrcDir </>) toc)
               zs <- mapM r (fmap (sbSrcDir </>) xs)
               void $ genBuildPageAction (sbSrcDir </> template)
                        (loadIfExists r . (-<.> ".md") . (sbSrcDir </>) . dropDirectory1)
                        (withData . withJSON (tocNavbarData (fmap enrichTypicalUrl ys)) . withSubsections (immediateShoots (enrichTypicalUrl <$> zs)))
                        out)

genDefaultIndexPageData :: [Value]
                        -> ([Value] -> [Value])
                        -> Text
                        -> (Text -> Text)
                        -> Int
                        -> Zipper [] Value
genDefaultIndexPageData xs f g h n  =  extend (genPageData g h) $ paginateWithFilter n f xs

getDefaultIndexPageData :: FilePath -> [FilePattern] -> Int -> Action (Zipper [] Value)
getDefaultIndexPageData dir pat postsPerPage = do
  xs <- getEnrichedPostData dir pat
  return $ genDefaultIndexPageData xs (id) "Posts" ("/posts/pages/" <>) postsPerPage

getDefaultTagPageData :: FilePath -> [FilePattern] -> Int -> Text -> Action (Zipper [] Value)
getDefaultTagPageData dir pat postsPerPage tag = do
  xs <- getEnrichedPostData dir pat
  return $ genDefaultIndexPageData xs (tagFilterPosts tag) ("Posts tagged " <> tag) (\x -> "/posts/tags/" <> tag <> "/pages/" <> x) postsPerPage

getDefaultMonthPageData :: FilePath -> [FilePattern] -> Int -> UTCTime -> Action (Zipper [] Value)
getDefaultMonthPageData dir pat postsPerPage time = do
  xs <- getEnrichedPostData dir pat
  return $ genDefaultIndexPageData xs (monthFilterPosts time) ("Posts from " <> T.pack (prettyMonthFormat time)) (\x -> "/posts/months/" <> T.pack (monthURLFormat time) <> "/pages/" <> x) postsPerPage

genDefaultPostIndexRule :: FilePath -> FilePattern -> (FilePattern -> Int) -> (FilePattern -> a) -> (a -> Action (Zipper [] Value)) -> Rules ()
genDefaultPostIndexRule dir fp f g h = comonadStoreRuleGen fp f g h
  (\a -> void <$> genBuildPageAction (dir </> "templates/post-list.html") (const $ return a) id)

defaultPostIndexPatterns :: [FilePattern] -> (Zipper [] Value -> Action (Zipper [] Value)) -> Shakebook ()
defaultPostIndexPatterns pat extData = Shakebook $ ask >>=
  \ShakebookConfig {sbSrcDir, sbOutDir, sbMdRead, sbHTWrite, sbPPP} -> lift $ do
     genDefaultPostIndexRule sbSrcDir (sbOutDir </> "posts/index.html")
                                      (const 0)
                                      (const ())
                                      (extData <=< const (getDefaultIndexPageData sbSrcDir pat sbPPP))
     genDefaultPostIndexRule sbSrcDir (sbOutDir </> "posts/pages/*/index.html")
                                      ((+ (-1)) . read . (!! 3) . splitOn "/")
                                      (const ())
                                      (extData <=< const (getDefaultIndexPageData sbSrcDir pat sbPPP))

defaultTagIndexPatterns :: [FilePattern] -> (Zipper [] Value -> Action (Zipper [] Value)) -> Shakebook ()
defaultTagIndexPatterns pat extData = Shakebook $ ask >>= 
  \ShakebookConfig {sbSrcDir, sbOutDir, sbMdRead, sbHTWrite, sbPPP} -> lift $ do
   genDefaultPostIndexRule sbSrcDir (sbOutDir </> "posts/tags/*/index.html")
                                    (const 0)
                                    (T.pack . (!! 3) . splitOn "/")
                                    (extData <=< getDefaultTagPageData sbSrcDir pat sbPPP)
   genDefaultPostIndexRule sbSrcDir (sbOutDir </> "posts/tags/*/pages/*/index.html")
                                    ((+ (-1)) . read . (!! 5) . splitOn "/")
                                    (T.pack . (!! 3) . splitOn "/") 
                                    (extData <=< getDefaultTagPageData sbSrcDir pat sbPPP)

defaultMonthIndexPatterns :: [FilePattern] -> (Zipper [] Value -> Action (Zipper [] Value)) -> Shakebook ()
defaultMonthIndexPatterns pat extData = Shakebook $ ask >>=
  \ShakebookConfig {sbSrcDir, sbOutDir, sbMdRead, sbHTWrite, sbPPP} -> lift $ do
     genDefaultPostIndexRule sbSrcDir (sbOutDir </> "posts/months/*/index.html") (const 0)
                                      (parseISODateTime . T.pack . (!! 3) . splitOn "/")
                                      (extData <=< getDefaultMonthPageData sbSrcDir pat sbPPP)
     genDefaultPostIndexRule sbSrcDir (sbOutDir </> "posts/months/*/pages/*/index.html")
                                      ((+ (-1)) . read . (!! 5) . splitOn "/")
                                      (parseISODateTime . T.pack . (!! 3) . splitOn "/")
                                      (extData <=< getDefaultMonthPageData sbSrcDir pat sbPPP)

defaultPostsPatterns :: FilePattern -> FilePath -> (Zipper [] Value -> Action (Zipper [] Value)) -> Shakebook ()
defaultPostsPatterns pat template extData = Shakebook $ ask >>=
  \ShakebookConfig {sbSrcDir, sbOutDir} -> 
    lift $ sbOutDir </> pat %> \out -> do
      xs <- getDirectoryFiles sbSrcDir [pat -<.> ".md"]
      ys <- sortAndFilterValues (fmap (sbSrcDir </>) xs) (Down . viewPostTime) (const True)
      let k = fromJust $ elemIndex ((-<.> ".md") . (sbSrcDir </>) . dropDirectory1 $ out) (fst <$> ys)
      let z = fromJust $ seek k <$> zipper (snd <$> ys)
      void $ genBuildPageAction (sbSrcDir </> template)
                                (const $ extract <$> extData z)
                                id out
    
  

buildPDF :: FilePath -> Cofree [] String -> Text -> FilePath -> Action ()
buildPDF srcDir toc baseUrl out = do
  y <- mapM readFile' ((srcDir </>) <$> toc)
  Right f <- liftIO . runIOorExplode $ do
    k <- mapM (readMarkdown markdownReaderOptions . T.pack) y
    let z = walk (handleImages (\x -> "[Video available at " <> baseUrl <> x <> "]")) $ foldr (<>) mempty $ pushHeaders (-1) k
    makePDFLaTeX z
  LBS.writeFile out f

copyStatic :: FilePath -> FilePath -> Action ()
copyStatic srcFolder out = do
  let src = srcFolder </> dropDirectory1 out
  copyFileChanged src out

data ShakebookConfig = ShakebookConfig {
   sbSrcDir  :: FilePath
,  sbOutDir  :: FilePath
,  sbMdRead  :: ReaderOptions
,  sbHTWrite :: WriterOptions
,  sbPPP :: Int
} deriving (Show)

newtype Shakebook a = Shakebook ( ReaderT ShakebookConfig Rules a )
  deriving (Functor, Applicative, Monad)

runShakebook :: ShakebookConfig -> Shakebook a -> Rules a
runShakebook c (Shakebook f) = runReaderT f c

defaultSinglePagePattern :: FilePath -- The output filename e.g "index.html".
                         -> FilePath -- A template file.
                         -> (Value -> Action Value) -- Last minute enrichment.
                         -> Shakebook ()
defaultSinglePagePattern fileName template withDataM = Shakebook $ ask >>=
  \ShakebookConfig {sbSrcDir, sbOutDir, sbMdRead, sbHTWrite} -> 
    lift $ sbOutDir </> fileName %> void . genBuildPageAction
             (sbSrcDir </> template)
             (withDataM <=< readMarkdownFile' sbMdRead sbHTWrite . (-<.> ".md") . (sbSrcDir </>) . dropDirectory1)
                    id

defaultStaticsPatterns :: FilePath -> FilePath -> Rules ()
defaultStaticsPatterns srcFolder outputFolder = do
  outputFolder </> "css//*"    %> copyStatic srcFolder
  outputFolder </> "images//*" %> copyStatic srcFolder
  outputFolder </> "js//*"     %> copyStatic srcFolder
  outputFolder </> "webfonts//*" %> copyStatic srcFolder

defaultCleanPhony :: FilePath -> Rules ()
defaultCleanPhony outputFolder = 
  phony "clean" $ do
      putInfo $ "Cleaning files in " ++ outputFolder
      removeFilesAfter outputFolder ["//*"]

defaultStaticsPhony :: FilePath -> FilePath -> Rules ()
defaultStaticsPhony srcFolder outputFolder = do
  phony "statics" $ do
    fp <- getDirectoryFiles srcFolder ["images//*", "css//*", "js//*", "webfonts//*"]
    need $ [outputFolder </> x | x <- fp]

defaultPostsPhony :: FilePath -> [FilePattern] -> FilePath -> Rules ()
defaultPostsPhony sourceFolder pattern outputFolder = do
  phony "posts" $ do
    fp <- getDirectoryFiles sourceFolder pattern
    need [outputFolder </> x -<.> ".html" | x <- fp]

defaultPostIndexPhony :: FilePath -> [FilePattern] -> FilePath -> Int -> Rules ()
defaultPostIndexPhony sourceFolder pattern outputFolder postsPerPage = 
    phony "posts-index" $ do
      fp <- defaultGetDirectoryMarkdown sourceFolder pattern
      need [outputFolder </> "posts/index.html"]
      need [outputFolder </> "posts/pages/" ++ show x ++ "/index.html"
           | x <- [1..size (fromJust $ paginate postsPerPage fp)]]

defaultTagIndexPhony :: FilePath -> [FilePattern] -> FilePath -> Int -> Rules ()
defaultTagIndexPhony sourceFolder pattern outputFolder postsPerPage = 
  phony "tag-index" $ do
    fp <- defaultGetDirectoryMarkdown sourceFolder pattern
    let tags = viewAllPostTags fp
    need [outputFolder </> "posts/tags" </> T.unpack x </> "index.html" | x <- tags]
    need [outputFolder </> "posts/tags" </> T.unpack x </> "pages" </> show p </> "index.html"
         |  x <- tags
         ,  p <- [1..size (fromJust $ paginate postsPerPage $ tagFilterPosts x fp)]
         ]

defaultMonthIndexPhony :: FilePath -> [FilePattern] -> FilePath -> Int -> Rules ()
defaultMonthIndexPhony sourceFolder pattern outputFolder postsPerPage = 
   phony "month-index" $ do
      fp <- defaultGetDirectoryMarkdown sourceFolder pattern
      let times = viewAllPostTimes fp
      need [outputFolder </> "posts/months" </> monthURLFormat t </> "index.html" | t <- times]
      need [outputFolder </> "posts/months" </> monthURLFormat t </> "pages" </> show p </> "index.html"
           | t <- times
           , p <- [1..length (fromJust $ paginate postsPerPage $ monthFilterPosts t fp)]
           ]

defaultDocsPhony :: FilePath -> Cofree [] String -> FilePath -> Rules ()
defaultDocsPhony sourceFolder toc outputFolder  = 
    phony "docs" $ do
      fp <- getDirectoryFiles sourceFolder (foldr ((<>) . pure) [] toc)
      need $ [ (outputFolder </>) . (-<.> ".html") $ x | x <- fp]
