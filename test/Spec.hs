{-# LANGUAGE TemplateHaskell #-}

import           Data.Aeson.With
import qualified Data.IxSet.Typed as Ix
import qualified Data.IxSet.Typed.Conversions as Ix
import           Data.List.Split
import           Data.Text.Time
import           Path.Extensions
import           RIO
import qualified RIO.HashMap       as HM
import           RIO.List
import           RIO.List.Partial
import           RIO.Partial
import qualified RIO.Text          as T
import           Shakebook
import           Test.Tasty
import           Test.Tasty.Golden

sourceFolder :: Path Rel Dir
sourceFolder = $(mkRelDir "test/site")

outputFolder :: Path Rel Dir
outputFolder = $(mkRelDir "test/public")

baseUrl :: Text
baseUrl = "http://blanky.test"

siteTitle :: Text
siteTitle = "Blanky Site"

tableOfContents :: Cofree [] (Path Rel File)
tableOfContents = $(mkRelFile "docs/index.md") :< [
                    $(mkRelFile "docs/1/index.md") :< []
                  , $(mkRelFile "docs/2/index.md") :< [
                  $(mkRelFile "docs/2/champ.md") :< []
                  ]
                ]

numRecentPosts :: Int
numRecentPosts = 3

numPageNeighbours :: Int
numPageNeighbours = 1

postsPerPage :: Int
postsPerPage = 5

mySocial :: [Value]
mySocial = uncurry genLinkData <$> [("twitter", "http://twitter.com/blanky-site-nowhere")
                                   ,("youtube", "http://youtube.com/blanky-site-nowhere")
                                   ,("gitlab", "http://gitlab.com/blanky-site-nowhere")]

myBlogNav :: Ix.IsIndexOf YearMonth xs => Ix.IxSet xs Post -> Value
myBlogNav = genBlogNavbarData "Blog" "/posts/" defaultPrettyMonthFormat defaultMonthUrlFragment

myDocNav :: Cofree [] Value -> Value
myDocNav = genTocNavbarData

myIndex :: MonadThrow m => [Post] -> m (Zipper [] Value)
myIndex = genIndexPageData "Posts" ("/posts/pages/" <>) postsPerPage

myTagIndex :: MonadThrow m => Tag -> [Post] -> m (Zipper [] Value)
myTagIndex (Tag t) = genIndexPageData ("Posts tagged " <> t) (("/posts/tags/" <> t <> "/pages/") <>) postsPerPage

myMonthIndex :: MonadThrow m => YearMonth -> [Post] -> m (Zipper [] Value)
myMonthIndex (YearMonth (y, m)) =
  let t' = fromYearMonthPair (y, m)
  in genIndexPageData (("Posts from " <>) . defaultPrettyMonthFormat $ t')
                      (("/posts/months/"  <> defaultMonthUrlFormat t' <> "/pages/") <>)
                      postsPerPage

rules :: HasLogFunc r => ShakePlus r ()
rules = do

  readMDC   <- newCache $ loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions

  postsIx   <- newCache $ postIndex $ readMDC >=> return . defaultEnrichPost

  blogNav   <- newCache $ postsIx >=> return . myBlogNav

  postsZ    <- newCache $ postsIx >=> postZipper

  blogIndex <- newCache $ postsIx >=> myIndex . Ix.toList

  blogTagIndex <- newCache $ postsIx >=> flip Ix.toHashMapByM myTagIndex

  blogMonthIndex <- newCache $ postsIx >=> flip Ix.toHashMapByM myMonthIndex

  let o' = (`within` outputFolder)
      s' = (`within` sourceFolder)

      myPosts = s' ["posts/*.md"]

      myBuildPage tmpl v out = do
        rs <- postsZ myPosts
        let v' = withHighlighting pygments
               . withSocialLinks mySocial
               . withSiteTitle siteTitle
               . withCdnImports defaultCdnImports
               . withRecentPosts (take numRecentPosts (unzipper rs)) $ v
        buildPageAction' (s' tmpl) v' out

      myBuildBlogPage tmpl v out = do
        k <- blogNav myPosts
        myBuildPage tmpl (withJSON k v) out

      myBuildPostListPage z out = do
        let v = extract . extendPageNeighbours numPageNeighbours $ z
        myBuildBlogPage $(mkRelFile "templates/post-list.html") v out

  o' "index.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    v   <- readMDC src
    myBuildPage $(mkRelFile "templates/index.html") v out

  o' "posts/*.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    xs  <- postsZ myPosts
    xs' <- seekOnThrow viewSrcPath (T.pack . toFilePath . extract $ src) xs
    myBuildBlogPage $(mkRelFile "templates/post.html") (toJSON $ extract xs') out

  toc' <- mapM (mapM withHtmlExtension) $ fmap o' tableOfContents
  void . sequence . flip extend toc' $ \xs -> (toFilePath <$> extract xs) %^> \out -> do
    let getDoc = readMDC <=< blinkAndMapM sourceFolder withMdExtension
    ys <- mapM getDoc toc'
    zs <- mapM getDoc (fmap extract . unwrap $ xs)
    v  <- getDoc out
    let v' = withJSON (myDocNav ys) . withSubsections zs $ v
    myBuildPage $(mkRelFile "templates/docs.html") v' out

  o' "posts/index.html" %^>
    copyFileChanged (o' ($(mkRelFile "posts/pages/1/index.html") :: Path Rel File))

  o' "posts/pages/*/index.html" %^> \out -> do
    let n = read . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs <- blogIndex myPosts
    myBuildPostListPage (seek (n -1) xs) out

  o' "posts/tags/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/tags/" <> t <> "/pages/1/index.html"
    copyFileChanged (o' i) out

  o' "posts/tags/*/pages/*/index.html" %^> \out -> do
     let zs = splitOn "/" . toFilePath . extract $ out
     let t = Tag $ T.pack $ zs !! 2
     let n = read   $ zs !! 4
     xs <- blogTagIndex myPosts
     case HM.lookup t xs of
       Nothing -> logError $ "Attempting to lookup non-existant tag " <> displayShow t
       Just x  -> myBuildPostListPage (seek (n - 1) x) out

  o' "posts/months/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/months/" <> t <> "/pages/1/index.html"
    copyFileChanged (o' i) out

  o' "posts/months/*/pages/*/index.html" %^> \out -> do
     let zs = splitOn "/" . toFilePath . extract $ out
     let t = YearMonth $ toYearMonthPair $ parseISODateTime $ T.pack $ zs !! 2
     let n = read   $ zs !! 4
     xs <- blogMonthIndex myPosts
     case HM.lookup t xs of
       Nothing -> logError $ "Attempting to lookup non-existant month " <> displayShow t
       Just x  -> myBuildPostListPage (seek (n - 1) x) out

  o' ["css//*", "js//*", "webfonts//*", "images//*"] |%^> \out ->
    copyFileChanged (blinkLocalDir sourceFolder out) out

  o' "sitemap.xml" %^> \out -> do
    xs <- postsZ myPosts
    buildSitemap baseUrl (unzipper xs) out

  let simplePipeline f = getDirectoryFiles sourceFolder >=> mapM f >=> needIn outputFolder
      verbatimPipeline = simplePipeline return

  phony "statics" $ verbatimPipeline ["css//*", "js//*", "webfonts//*", "images//*"]

  phony "index" $ needIn outputFolder [$(mkRelFile "index.html") :: Path Rel File]

  phony "post-index" $ do
     ps <- blogIndex myPosts
     fs <- defaultPagePaths [1..size ps]
     let postFolder = (outputFolder </> $(mkRelDir "posts") :: Path Rel Dir)
     needIn postFolder ($(mkRelFile "index.html") : fs)

  phony "by-tag-index" $ do
     ps <- blogTagIndex myPosts
     void $ flip HM.traverseWithKey ps $ \(Tag t) z -> do
       u  <- parseRelDir $ T.unpack t
       fs <- defaultPagePaths [1..size z]
       let tagFolder = outputFolder </> $(mkRelDir "posts/tags") </> u
       needIn tagFolder ($(mkRelFile "index.html") : fs)

  phony "by-month-index" $ do
     ps <- blogMonthIndex myPosts
     void $ flip HM.traverseWithKey ps $ \(YearMonth t) z -> do
       u  <- parseRelDir $ T.unpack $ defaultMonthUrlFormat $ fromYearMonthPair $ t
       fs <- defaultPagePaths [1..size z]
       let monthFolder = outputFolder </> $(mkRelDir "posts/months") </> u
       needIn monthFolder ($(mkRelFile "index.html") : fs)

  phony "docs" $
    mapM withHtmlExtension tableOfContents >>= needIn outputFolder

  phony "posts" $ simplePipeline withHtmlExtension ["posts/*.md"]

  phony "clean" $ do
    logInfo $ "Cleaning files in " <> displayShow outputFolder
    removeFilesAfter outputFolder ["//*"]

  phony "sitemap" $ needIn outputFolder [$(mkRelFile "sitemap.xml") :: Path Rel File]

tests :: [FilePath] -> TestTree
tests xs = testGroup "Rendering Tests" $
  map ( \x -> goldenVsFile x x
     (replace "golden" "public" x)
     (return ())) xs
  where replace fr to' = intercalate to' . splitOn fr

main :: IO ()
main = do
   xs <- findByExtension [".html", ".xml"] "test/golden"
   logOptions' <- logOptionsHandle stdout True
   (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo logOptions')
   shake shakeOptions $ want ["clean"] >> runShakePlus lf rules
   shake shakeOptions $ want ["index", "docs", "posts", "post-index", "by-tag-index", "by-month-index", "sitemap"] >> runShakePlus lf rules
   defaultMain $ tests xs
   dlf
