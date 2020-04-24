{-# LANGUAGE BlockArguments #-}
module Shakebook.Defaults where
                                                                                                        

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Store.Class
import           Control.Comonad.Store.Zipper
import           Control.Lens               hiding ((:<), Context)
import           Control.Monad.Extra
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake          as S
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.Forward
import           GHC.Generics               (Generic)
import           RIO                        hiding (view)
import qualified RIO.ByteString.Lazy        as LBS
import           RIO.Directory
import qualified RIO.HashMap                as HML
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Map                    as M
import           RIO.Partial
import qualified RIO.Text                   as T
import qualified RIO.Text.Partial           as T
import qualified RIO.Text.Lazy              as TL
import           RIO.Time
import qualified RIO.Vector                 as V
import           Slick
import           Shakebook.Data
import           Shakebook.Rules
import           Slick.Pandoc
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
enrichPost = enrichTeaser "<!--more-->" . enrichTagLinks ("/posts/tags/" <>) . enrichPrettyDate prettyTimeFormat

--Data models-------------------------------------------------------------------

getMarkdownData' = getMarkdownData markdownReaderOptions html5WriterOptions

withNavCover :: Value -> Value
withNavCover = withStringField "nav-class" "td-navbar-cover"

getAllPostData :: Action [Value]
getAllPostData = getDirectoryFiles "" ["site/posts/*.md"] >>= mapM (fmap enrichPost . getMarkdownData')

discoverAllPostTags :: Action [Text]
discoverAllPostTags = viewAllPostTags <$> getAllPostData

discoverAllPostTimes :: Action [UTCTime]
discoverAllPostTimes = viewAllPostTimes <$> getAllPostData


markdownReaderOptions :: ReaderOptions
markdownReaderOptions = def { readerExtensions = pandocExtensions }

html5WriterOptions :: WriterOptions
html5WriterOptions = def { writerHTMLMathMethod = MathJax ""}

latexWriterOptions :: WriterOptions
latexWriterOptions = def { writerTableOfContents = True, writerVariables = Context (M.fromList [("geometry",SimpleVal "margin=3cm"), ("fontsize", (SimpleVal "10")), ("linkcolor",SimpleVal "blue")]) }

makePDFLaTeX :: Pandoc -> PandocIO (Either LBS.ByteString LBS.ByteString)
makePDFLaTeX p = do
  t <- compileDefaultTemplate "latex"
  makePDF "pdflatex" [] writeLaTeX latexWriterOptions { writerTemplate = Just t } p

handleImages :: (Text -> Text) -> Inline -> Inline
handleImages f x@(Image attr ins (src,txt)) =
  if T.takeEnd 4 src == ".mp4" then Str (f src)
  else Image attr ins ("public/" <> src, txt)
handleImages f x = x

handleHeaders :: Int -> Block -> Block
handleHeaders i x@(Header a as xs) = Header (max 1 (a + i)) as xs
handleHeaders _ x                  = x

pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

generateBuildDocAction toc f xs out = do
  ys <- mapM getMarkdownData' toc
  zs <- mapM getMarkdownData' xs
  void $ genBuildPageAction "site/templates/docs.html"
                            (typicalMarkdownGet getMarkdownData' "site")
                            (f . withJSON (tocNavbarData ys) . withSubsections (immediateShoots zs))
                            out

generateAllDocRules outputFolder toc f = cofreeRuleGen toc (\x -> outputFolder </> typicalHTMLPath x) (generateBuildDocAction toc f)


genIndexPageData f g h n m = do
  xs <- getAllPostData
  let ys = paginateWithFilter n f xs
  return $ fmap (withJSON (myBlogNavbar xs)) . extendPageNeighbours m . extend (genPageData g h) $ ys

indexPageData :: Action (Zipper [] Value)
indexPageData = genIndexPageData id ("Posts") (\x -> ("/posts/pages/" <> x)) 5 2

tagPageData :: Text -> Action (Zipper [] Value)
tagPageData tag = genIndexPageData (tagFilterPosts tag) ("Posts tagged " <> tag) (\x -> "/posts/tags/" <> tag <> "/pages/" <> x) 5 2

monthPageData :: UTCTime -> Action (Zipper [] Value)
monthPageData time = genIndexPageData (monthFilterPosts time) ("Posts from " <> T.pack (prettyMonthFormat time)) ((\x -> "/posts/months/" <> T.pack (monthURLFormat time) <> "/pages/" <> x)) 5 2

genPostIndexRule :: FilePath -> (FilePattern -> Int) -> (FilePattern -> a) -> (a -> Action (Zipper [] Value)) -> Rules ()
genPostIndexRule fp f g h = comonadStoreRuleGen fp f g h
  (\a -> void <$> genBuildPageAction "site/templates/post-list.html" (const $ return $ a) (withHighlighting pygments))

postIndexRules :: FilePath -> Rules ()
postIndexRules outputFolder = do
   genPostIndexRule (outputFolder </> "posts/index.html") (const 0) (const ()) (const indexPageData)
   genPostIndexRule (outputFolder </> "posts/pages/*/index.html") ((+ (-1)) . read . (!! 3) . splitOn "/")
                                                             (const ())
                                                             (const indexPageData)
   genPostIndexRule (outputFolder </> "posts/tags/*/index.html") (const 0)
                                                            (T.pack . (!! 3) . splitOn "/")
                                                            tagPageData
   genPostIndexRule (outputFolder </> "posts/tags/*/pages/*/index.html") ((+ (-1)) . read . (!! 5) . splitOn "/")
                                                 (T.pack . (!! 3) . splitOn "/")
                                                 tagPageData
   genPostIndexRule (outputFolder </> "posts/months/*/index.html") (const 0)
                                           (parseISODateTime . T.pack . (!! 3) . splitOn "/")
                                           monthPageData
   genPostIndexRule (outputFolder </> "posts/months/*/pages/*/index.html") ((+ (-1)) . read . (!! 5) . splitOn "/")
                                                   (parseISODateTime . T.pack . (!! 3) . splitOn "/")
                                                   monthPageData

buildPDF :: Cofree [] String -> Text -> FilePath -> Action ()
buildPDF toc baseUrl out = do
  y <- mapM readFile' toc
  Right f <- liftIO . runIOorExplode $ do
    k <- mapM (readMarkdown markdownReaderOptions . T.pack) y
    let z = walk (handleImages (\x -> "[Video available at " <> baseUrl <> x <> "]")) $ foldr (<>) mempty $ pushHeaders (-1) k
    makePDFLaTeX z
  LBS.writeFile out f

copyStatic :: FilePath -> Action ()
copyStatic out = do
  let src = "site" </> dropDirectory1 out
  copyFileChanged src out

buildPost :: FilePath -> Action ()
buildPost out = do
  xs <- getAllPostData
  void $ genBuildPageAction "site/templates/post.html"
                            (typicalMarkdownGet getMarkdownData' "site")
                            ( enrichPost . withJSON (myBlogNavbar xs))
                            out


staticRules outputFolder = do
    outputFolder </> "css//*"    %> copyStatic
    outputFolder </> "images//*" %> copyStatic
    outputFolder </> "js//*"     %> copyStatic
    outputFolder </> "webfonts//*" %> copyStatic


phonyPostIndexRules :: FilePath -> Int -> Rules ()
phonyPostIndexRules outputFolder postsPerPage = 
    phony "posts" $ do
      fp <- getDirectoryFiles "" ["site/posts/*.md"]
      need ["public/posts/index.html"]
      need [outputFolder </> "posts/pages/" ++ show x ++ "/index.html" | x <- [1..size (fromJust $ paginate postsPerPage fp)]]
      need [outputFolder </> dropDirectory1 x -<.> ".html" | x <- fp]


phonyTagIndexRules outputFolder postsPerPage = 
      phony "tags" $ do
      tags <- discoverAllPostTags
      need [outputFolder </> "posts/tags" </> T.unpack x </> "index.html" | x <- tags]
      xs <- getAllPostData
      need [outputFolder </> "posts/tags" </> T.unpack x </> "pages" </> (show p) </> "index.html"
           |  x <- tags
           ,  p <- [1..size (fromJust $ paginate postsPerPage $ tagFilterPosts x xs)]
           ]


phonyMonthIndexRules outputFolder postsPerPage =
   phony "months" $ do
      times <- discoverAllPostTimes
      need [outputFolder </> "posts/months" </> monthURLFormat t </> "index.html" | t <- times]
      xs <- getAllPostData
      need [outputFolder </> "posts/months" </> monthURLFormat t </> "pages" </> show p </> "index.html"
           | t <- times
           , p <- [1..length (fromJust $ paginate postsPerPage $ monthFilterPosts t xs)]
           ]

phonyDocsRules outputFolder toc = 
    phony "docs" $ do
      fp <- getDirectoryFiles "" (foldr ((<>) . pure) [] toc)
      need $ [ outputFolder </> typicalHTMLPath x | x <- fp]

