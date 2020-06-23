{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens      hiding ((:<))
import           Data.Aeson.With
import qualified Data.IxSet        as Ix
import           Data.List.Split
import           Data.Text.Time
import           Path.Extensions
import           RIO
import qualified RIO.HashMap       as HM
import           RIO.List
import           RIO.List.Partial
import           RIO.Partial
import qualified RIO.Text          as T
import           RIO.Time
import           Shakebook
import           Test.Tasty
import           Test.Tasty.Golden

sourceFolder :: Path Rel Dir
sourceFolder = $(mkRelDir "test/site")

outputFolder :: Path Rel Dir
outputFolder = $(mkRelDir "test/public")

--baseUrl :: Text
--baseUrl = "http://blanky.test"

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

rules :: HasLogFunc r => ShakePlus r ()
rules = do

  readMDC <- newCache $ loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions

  postsIx <- newCache $ \fp -> do
    xs <- batchLoadWithin' fp readMDC
    return $ Ix.fromList $ Post <$> HM.elems (defaultEnrichPost <$> xs)

  getBlogNavbar <- newCache $ \fp -> do
    xs <- postsIx fp
    return $ genBlogNavbarData "Blog" "/posts/" defaultPrettyMonthFormat defaultMonthUrlFragment xs

  postsZ <- newCache $ \fp -> do
    xs <- postsIx fp
    let ys = Ix.toDescList (Ix.Proxy :: Ix.Proxy Posted) xs
    zs <- ifor ys $ \i v -> do
      z <- zipper' (unPost <$> ys)
      return (viewSrcPath (unPost v), seek i z)
    return $ HM.fromList zs

  blogIndexPageData <- newCache $ \fp -> do
    xs <- postsIx fp
    genIndexPageData (unPost <$> Ix.toList xs) "Posts" ("/posts/pages/" <>) postsPerPage

  blogTagIndexPageData <- newCache $ \fp -> do
    xs <- postsIx fp
    k <- forM (Ix.groupDescBy xs) $ \((Tag t), ys) -> do
      z <- genIndexPageData (unPost <$> ys) ("Posts tagged " <> t) (("/posts/tags/" <> t <> "/pages/") <>) postsPerPage
      return (t, z)
    return $ HM.fromList k

  blogMonthIndexPageData <- newCache $ \fp -> do
    xs <- postsIx fp
    k <- forM (Ix.groupDescBy xs) $ \(YearMonth (y,m), ys) -> do
      let t' = UTCTime (fromGregorian y m 1) 0
      z <- genIndexPageData (unPost <$> ys)
                            (("Posts from " <>) . defaultPrettyMonthFormat $ t')
                            (("/posts/months/"  <> defaultMonthUrlFormat t' <> "/pages/") <>)
                            postsPerPage
      return (defaultMonthUrlFormat t', z)
    return $ HM.fromList k

  let myPosts = ["posts/*.md"] `within` sourceFolder

      o' = (`within` outputFolder)
      s' = (`within` sourceFolder)

      myBuildPage tmpl v out = do
        rs <- postsIx myPosts
        let v' = withHighlighting pygments
               . withSocialLinks mySocial
               . withSiteTitle siteTitle
               . withRecentPosts (take numRecentPosts $ Ix.toDescList (Ix.Proxy :: Ix.Proxy Posted) rs) $ v
        buildPageActionWithin (s' tmpl) v' out

      myBuildBlogPage tmpl v out = do
        k <- getBlogNavbar myPosts
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
    xs <- postsZ myPosts
    case HM.lookup (T.pack . toFilePath . extract $ src) xs of
      Nothing -> logError $ "Attempting to lookup non-existent post " <> displayShow src
      Just x  -> myBuildBlogPage $(mkRelFile "templates/post.html") (extract x) out

  toc' <- mapM (mapM withHtmlExtension) $ fmap o' tableOfContents
  void . sequence . flip extend toc' $ \xs -> (toFilePath <$> extract xs) %^> \out -> do
    let getDoc = readMDC <=< blinkAndMapM sourceFolder withMdExtension
    ys <- mapM getDoc toc'
    zs <- mapM getDoc (fmap extract . unwrap $ xs)
    v  <- getDoc out
    let v' = withJSON (genTocNavbarData ys) . withSubsections zs $ v
    myBuildPage $(mkRelFile "templates/docs.html") v' out

  o' "posts/index.html" %^>
    copyFileChangedWithin (o' $(mkRelFile "posts/pages/1/index.html"))

  o' "posts/pages/*/index.html" %^> \out -> do
    let n = (+ (-1)) . read . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs <- blogIndexPageData myPosts
    myBuildPostListPage (seek n xs) out

  o' "posts/tags/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/tags/" <> t <> "/pages/1/index.html"
    copyFileChangedWithin (o' i) out

  o' "posts/tags/*/pages/*/index.html" %^> \out -> do
     let t = T.pack          . (!! 2) . splitOn "/" . toFilePath . extract $ out
     let n = (+ (-1)) . read . (!! 4) . splitOn "/" . toFilePath . extract $ out
     xs <- blogTagIndexPageData myPosts
     case HM.lookup t xs of
       Nothing -> logError $ "Attempting to lookup non-existant tag " <> display t
       Just x  -> myBuildPostListPage (seek n x) out

  o' "posts/months/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/months/" <> t <> "/pages/1/index.html"
    copyFileChangedWithin (o' i) out

  o' "posts/months/*/pages/*/index.html" %^> \out -> do
     let t = T.pack . (!! 2) . splitOn "/" . toFilePath . extract $ out
     let n = (+ (-1)) . read  . (!! 4) . splitOn "/" . toFilePath . extract $ out
     xs <- blogMonthIndexPageData myPosts
     case HM.lookup t xs of
       Nothing -> logError $ "Attempting to lookup non-existant month " <> displayShow t
       Just x  -> myBuildPostListPage (seek n x) out

  o' ["css//*", "js//*", "webfonts//*", "images//*"] |%^> \out ->
    copyFileChangedWithin (blinkLocalDir sourceFolder out) out

  let simplePipeline f = getDirectoryFiles sourceFolder >=> mapM f >=> needIn outputFolder
      verbatimPipeline = simplePipeline return

  phony "statics" $ verbatimPipeline ["css//*", "js//*", "webfonts//*", "images//*"]

  phony "index" $ needIn outputFolder [$(mkRelFile "index.html")]

  phony "post-index" $ do
     ps <- blogIndexPageData myPosts
     fs <- defaultPagePaths [1..size ps]
     needIn (outputFolder </> $(mkRelDir "posts")) ($(mkRelFile "index.html") : fs)

  phony "by-tag-index" $ do
     ps <- blogTagIndexPageData myPosts
     void $ flip HM.traverseWithKey ps $ \t z -> do
       u  <- parseRelDir $ T.unpack t
       fs <- defaultPagePaths [1..size z]
       let tagFolder = outputFolder </> $(mkRelDir "posts/tags") </> u
       needIn tagFolder ($(mkRelFile "index.html") : fs)

  phony "by-month-index" $ do
     ps <- blogMonthIndexPageData myPosts
     void $ flip HM.traverseWithKey ps $ \t z -> do
       u  <- parseRelDir $ T.unpack t
       fs <- defaultPagePaths [1..size z]
       let monthFolder = outputFolder </> $(mkRelDir "posts/months") </> u
       needIn monthFolder ($(mkRelFile "index.html") : fs)

  phony "docs" $
    mapM withHtmlExtension tableOfContents >>= needIn outputFolder

  phony "posts" $ simplePipeline withHtmlExtension ["posts/*.md"]

  phony "clean" $ do
    logInfo $ "Cleaning files in " <> displayShow outputFolder
    removeFilesAfter outputFolder ["//*"]

tests :: [FilePath] -> TestTree
tests xs = testGroup "Rendering Tests" $
  map ( \x -> goldenVsFile x x
     (replace "golden" "public" x)
     (return ())) xs
  where replace fr to' = intercalate to' . splitOn fr

main :: IO ()
main = do
   xs <- findByExtension [".html"] "test/golden"
   logOptions' <- logOptionsHandle stdout True
   (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo logOptions')
   shake shakeOptions $ want ["clean"] >> runShakePlus lf rules
   shake shakeOptions $ want ["index", "docs", "posts", "post-index", "by-tag-index", "by-month-index"] >> runShakePlus lf rules
   defaultMain $ tests xs
   dlf
