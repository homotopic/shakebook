{-# LANGUAGE TemplateHaskell #-}

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Comonad.Zipper.Extra
import           Control.Lens hiding ((:<))
import           Data.Aeson
import           Data.Aeson.With
import           Data.Hashable.Time()
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake.Plus
import           Path.Extensions
import           RIO
import           RIO.Partial
import qualified RIO.HashMap                  as HM
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Text                     as T
import           Shakebook.Data
import           Shakebook.Defaults
import           Shakebook.Mustache
import           Shakebook.Conventions
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.Pandoc.Highlighting

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

  postsC  <- newCache $ \w -> do
    xs <- batchLoadWithin' w readMDC
    return $ defaultEnrichPost <$> xs

  sortedPosts <- newCache $ \fp -> do
    allPosts <- postsC fp
    return $  sortOn (Down . viewPostTime . snd) $ HM.toList allPosts

  getRecentPosts <- newCache $ \fp -> do
    xs <- sortedPosts fp
    return $ take numRecentPosts (snd <$> xs)

  getBlogNavbar <- newCache $ \fp -> do
    allPosts <- postsC fp
    return $ genBlogNavbarData "Blog" "/posts/"
                  (T.pack . defaultPrettyMonthFormat)
                  defaultMonthUrlFragment (HM.elems allPosts)

  postsZ <- newCache $ \fp -> do
    xs <- sortedPosts fp
    zs <- ifor xs $ \i (k, _) -> do
      z <- zipper' (snd <$> xs)
      return (k, seek i z)
    return $ HM.fromList zs

  blogIndexPageData <- newCache $ \fp -> do
    xs <- sortedPosts fp
    genIndexPageData (snd <$> xs) "Posts" ("/posts/pages/" <>) postsPerPage

  blogTagIndexPageData <- newCache $ \fp -> do
    xs <- sortedPosts fp
    zs <- forM (viewAllPostTags (snd <$> xs)) $ \t -> do
      let ys = filter (elem t . viewTags . snd) xs
      k <- genIndexPageData (snd <$> ys) ("Posts tagged " <> t) (("/posts/tags/" <> t <> "/pages/") <>) postsPerPage
      return (t, k)
    return $ HM.fromList zs

  blogMonthIndexPageData <- newCache $ \fp -> do
    xs <- sortedPosts fp
    zs <- forM (nub $ defaultMonthUrlFormat <$> viewAllPostTimes (snd <$> xs)) $ \t -> do
      let t' = parseISODateTime . T.pack $ t
      let ys = filter (sameMonth t' . viewPostTime . snd) xs
      k <- genIndexPageData (snd <$> ys)
                   (("Posts from " <>) . T.pack . defaultPrettyMonthFormat $ t')
                   (("/posts/months/"  <> T.pack (defaultMonthUrlFormat t') <> "/pages/") <>) postsPerPage
      return (t', k)
    return $ HM.fromList zs

  let myPosts = ["posts/*.md"] `within` sourceFolder

      o' = (`within` outputFolder)
      s' = (`within` sourceFolder)

      myBuildPage tmpl v out = do
        rs <- getRecentPosts myPosts
        let v' = withHighlighting pygments
               . withSocialLinks mySocial
               . withSiteTitle siteTitle
               . withRecentPosts rs $ v
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
    case HM.lookup src xs of
      Nothing -> logError $ "Attempting to lookup non-existent post " <> display (WithinDisplay src)
      Just x  -> myBuildBlogPage $(mkRelFile "templates/post.html") (extract x) out

  toc' <- mapM (mapM withHtmlExtension) $ fmap o' tableOfContents
  void . sequence . flip extend toc' $ \xs -> (toFilePath <$> extract xs) %^> \out -> do
    let getDoc = readMDC <=< blinkAndMapM sourceFolder withMdExtension 
    ys <- mapM getDoc toc'
    zs <- mapM getDoc (immediateShoots xs)
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
     let t = parseISODateTime . T.pack . (!! 2) . splitOn "/" . toFilePath . extract $ out
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
       u  <- parseRelDir $ defaultMonthUrlFormat t
       fs <- defaultPagePaths [1..size z]
       let monthFolder = outputFolder </> $(mkRelDir "posts/months") </> u
       needIn monthFolder ($(mkRelFile "index.html") : fs)

  phony "docs" $
    mapM withHtmlExtension tableOfContents >>= needIn outputFolder

  phony "posts" $ simplePipeline withHtmlExtension ["posts/*.md"]

  phony "clean" $ do
    logInfo $ "Cleaning files in " <> display (PathDisplay outputFolder)
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
