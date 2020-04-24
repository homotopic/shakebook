module Shakebook.Defaults where
                                                                                                        

import           Control.Comonad
import           Control.Comonad.Cofree
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
import           Text.Pandoc.Highlighting
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

monthIndexUrl :: UTCTime -> String
monthIndexUrl t = "/posts/months" </> monthURLFormat t

myBlogNavbar :: [Value] -> Value
myBlogNavbar = blogNavbarData "Blog" "/posts/" (T.pack . prettyMonthFormat) (T.pack . monthIndexUrl)

enrichPost :: Value -> Value
enrichPost = enrichTeaser "<!--more-->"
           . enrichTagLinks ("/posts/tags/" <>)
           . enrichPrettyDate prettyTimeFormat
           . enrichTypicalUrl

--Data models-------------------------------------------------------------------

withNavCover :: Value -> Value
withNavCover = withStringField "nav-class" "td-navbar-cover"

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
latexWriterOptions = def { writerTableOfContents = True, writerVariables = Context (M.fromList [("geometry",SimpleVal "margin=3cm"), ("fontsize", SimpleVal "10"), ("linkcolor",SimpleVal "blue")]) }

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

genDefaultDocAction :: FilePath
                    -> Cofree [] FilePath
                    -> (Value -> Value)
                    -> Cofree [] FilePath
                    -> FilePath
                    -> Action ()
genDefaultDocAction sourceFolder toc f xs out = do
  ys <- mapM defaultReadMarkdownFile toc
  zs <- mapM defaultReadMarkdownFile xs
  void $ genBuildPageAction (sourceFolder </> "templates/docs.html")
                            (loadIfExists defaultReadMarkdownFile . (-<.> ".md") . (sourceFolder </>) . dropDirectory1)
                            (f . withJSON (tocNavbarData ys) . withSubsections (immediateShoots zs))
                            out

genDefaultDocsAction :: FilePath -> FilePath -> Cofree [] FilePath -> (Value -> Value) -> Rules ()
genDefaultDocsAction dir outputFolder toc f = cofreeRuleGen toc (\x -> outputFolder </> typicalHTMLPath x) (genDefaultDocAction dir toc f)

enrichPages :: [Value] -> Int -> Zipper [] Value -> Zipper [] Value
enrichPages xs n = fmap (withJSON (myBlogNavbar xs)) . extendPageNeighbours n

genDefaultIndexPageData :: [Value]
                        -> ([Value] -> [Value])
                        -> Text
                        -> (Text -> Text)
                        -> Int
                        -> Int
                        -> Zipper [] Value
genDefaultIndexPageData xs f g h n m = enrichPages xs m . extend (genPageData g h) $ paginateWithFilter n f xs

getDefaultIndexPageData :: FilePath -> [FilePattern] -> Action (Zipper [] Value)
getDefaultIndexPageData dir pat = getEnrichedPostData dir pat >>= return . \a -> genDefaultIndexPageData a (id) "Posts" ("/posts/pages/" <>) 5 2

getDefaultTagPageData :: FilePath -> [FilePattern] -> Text -> Action (Zipper [] Value)
getDefaultTagPageData dir pat  tag = getEnrichedPostData dir pat >>= return . \a -> genDefaultIndexPageData a (tagFilterPosts tag) ("Posts tagged " <> tag) (\x -> "/posts/tags/" <> tag <> "/pages/" <> x) 5 2

getDefaultMonthPageData :: FilePath -> [FilePattern] -> UTCTime -> Action (Zipper [] Value)
getDefaultMonthPageData dir pat time = getEnrichedPostData dir pat >>= return . \a -> genDefaultIndexPageData a (monthFilterPosts time) ("Posts from " <> T.pack (prettyMonthFormat time)) (\x -> "/posts/months/" <> T.pack (monthURLFormat time) <> "/pages/" <> x) 5 2

genDefaultPostIndexRule :: FilePath -> FilePath -> (FilePattern -> Int) -> (FilePattern -> a) -> (a -> Action (Zipper [] Value)) -> Rules ()
genDefaultPostIndexRule dir fp f g h = comonadStoreRuleGen fp f g h
  (\a -> void <$> genBuildPageAction (dir </> "templates/post-list.html") (const $ return a) (withHighlighting pygments))

defaultPostIndexPatterns :: FilePath -> [FilePattern] -> FilePath -> Rules ()
defaultPostIndexPatterns dir pat outputFolder = do
   genDefaultPostIndexRule dir (outputFolder </> "posts/index.html") (const 0) (const ()) (const (getDefaultIndexPageData dir pat))
   genDefaultPostIndexRule dir (outputFolder </> "posts/pages/*/index.html") ((+ (-1)) . read . (!! 3) . splitOn "/")
                                                             (const ())
                                                             (const (getDefaultIndexPageData dir pat))
   genDefaultPostIndexRule dir (outputFolder </> "posts/tags/*/index.html") (const 0)
                                                            (T.pack . (!! 3) . splitOn "/")
                                                            (getDefaultTagPageData dir pat)
   genDefaultPostIndexRule dir (outputFolder </> "posts/tags/*/pages/*/index.html") ((+ (-1)) . read . (!! 5) . splitOn "/")
                                                 (T.pack . (!! 3) . splitOn "/") 
                                                            (getDefaultTagPageData dir pat)
   genDefaultPostIndexRule dir (outputFolder </> "posts/months/*/index.html") (const 0)
                                           (parseISODateTime . T.pack . (!! 3) . splitOn "/")
                                                            (getDefaultMonthPageData dir pat)
   genDefaultPostIndexRule dir (outputFolder </> "posts/months/*/pages/*/index.html") ((+ (-1)) . read . (!! 5) . splitOn "/")
                                                   (parseISODateTime . T.pack . (!! 3) . splitOn "/")
                                                            (getDefaultMonthPageData dir pat)

buildPDF :: Cofree [] String -> Text -> FilePath -> Action ()
buildPDF toc baseUrl out = do
  y <- mapM readFile' toc
  Right f <- liftIO . runIOorExplode $ do
    k <- mapM (readMarkdown markdownReaderOptions . T.pack) y
    let z = walk (handleImages (\x -> "[Video available at " <> baseUrl <> x <> "]")) $ foldr (<>) mempty $ pushHeaders (-1) k
    makePDFLaTeX z
  LBS.writeFile out f

copyStatic :: FilePath -> FilePath -> Action ()
copyStatic srcFolder out = do
  let src = srcFolder </> dropDirectory1 out
  copyFileChanged src out

defaultBuildPost :: FilePath -> [FilePattern] -> FilePath -> Action ()
defaultBuildPost dir pat out = do
  xs <- defaultGetDirectoryMarkdown dir pat
  putNormal out
  putNormal $ (-<.> ".md") . (dir </>) $ out
  void $ genBuildPageAction (dir </> "templates/post.html")
                            (loadIfExists defaultReadMarkdownFile . (-<.> ".md") . (dir </>) . dropDirectory1)
                            ( enrichPost . withJSON (myBlogNavbar xs))
                            out

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

defaultDocsPhony :: FilePath -> Cofree [] String -> Rules ()
defaultDocsPhony outputFolder toc = 
    phony "docs" $ do
      fp <- getDirectoryFiles "" (foldr ((<>) . pure) [] toc)
      need $ [ outputFolder </> typicalHTMLPath x | x <- fp]

