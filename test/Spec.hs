{-# LANGUAGE TemplateHaskell #-}

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Comonad.Zipper.Extra
import           Data.Aeson
import           Data.Aeson.With
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake.Plus
import           Path
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
import           Within

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

--numPageNeighbours :: Int
--numPageNeighbours = 1

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
                  (defaultMonthUrlFragment) (HM.elems allPosts)

  let myPosts = ["posts/*.md"] `within` sourceFolder

      myBuildPage tmpl v out = do
        rs <- getRecentPosts myPosts
        let v' = withHighlighting pygments
               . withSocialLinks mySocial
               . withSiteTitle siteTitle
               . withRecentPosts rs $ v
        buildPageActionWithin (tmpl `within` sourceFolder) v' out


  ("index.html" `within` outputFolder) %^> \out -> do
    src <- blinkAndMapM sourceFolder withMarkdownExtension $ out
    v   <- readMDC src
    myBuildPage $(mkRelFile "templates/index.html") v out

  ("posts/*.html" `within` outputFolder) %^> \out -> do
    src <- blinkAndMapM sourceFolder withMarkdownExtension $ out
    xs <- sortedPosts myPosts
    let k = elemIndex src (fst <$> xs)
    let z = fromJust $ liftA2 seek k $ zipper (snd <$> xs)
    n   <- getBlogNavbar myPosts
    let v' = withJSON n $ (extract z) 
    myBuildPage $(mkRelFile "templates/post.html") v' out

  toc' <- mapM (mapM withHtmlExtension) $ fmap (`within` outputFolder) tableOfContents
  void . sequence . flip extend toc' $ \xs -> (fmap toFilePath $ extract xs) %^> \out -> do
    let getDoc = readMDC <=< blinkAndMapM sourceFolder withMarkdownExtension 
    ys <- mapM getDoc toc'
    zs <- mapM getDoc $ (immediateShoots xs)
    v  <- getDoc $ out
    let v' = withJSON (genTocNavbarData ys) . withSubsections zs $ v
    myBuildPage $(mkRelFile "templates/docs.html") v' out

  ("posts/index.html" `within` outputFolder) %^>
    copyFileChangedWithin ($(mkRelFile "posts/pages/1/index.html") `within` outputFolder)

  ("posts/pages/*/index.html" `within` outputFolder) %^> \out -> do
    xs <- sortedPosts myPosts
    let n = (+ (-1)) . read . (!! 2) . splitOn "/" . toFilePath . extract $ out
    p <- seek n <$> genIndexPageData (snd <$> xs) "Posts" ("/posts/pages" <>) postsPerPage
    k <- getBlogNavbar myPosts
    let v = withJSON k $ extract p
    myBuildPage $(mkRelFile "templates/post-list.html") v out
 
  ("posts/tags/*/index.html" `within` outputFolder) %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/tags/" <> t <> "/pages/1/index.html"
    copyFileChangedWithin (i `within` outputFolder) out

  ("posts/tags/*/pages/*/index.html" `within` outputFolder) %^> \out -> do
    let t = T.pack          . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs <- filter (elem t . viewTags . snd) <$> sortedPosts myPosts
    let n = (+ (-1)) . read . (!! 4) . splitOn "/" . toFilePath . extract $ out
    p  <- seek n <$> genIndexPageData (snd <$> xs) ("Posts tagged " <> t) (("/posts/tags/"  <> t <> "/posts") <>) postsPerPage
    k <- getBlogNavbar myPosts
    let v = withJSON k $ extract p
    myBuildPage $(mkRelFile "templates/post-list.html") v out

  ("posts/months/*/index.html" `within` outputFolder) %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/months/" <> t <> "/pages/1/index.html"
    copyFileChangedWithin (i `within` outputFolder) out

  ("posts/months/*/pages/*/index.html" `within` outputFolder) %^> \out -> do
    let t = parseISODateTime . T.pack . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs <- filter (sameMonth t . viewPostTime . snd) <$> sortedPosts myPosts
    let n = (+ (-1)) . read  . (!! 4) . splitOn "/" . toFilePath . extract $ out
    p  <- seek n <$> genIndexPageData (snd <$> xs)
                       (("Posts from " <>) . T.pack . defaultPrettyMonthFormat $ t)
                       (("/posts/months/"  <> T.pack (defaultMonthUrlFormat t) <> "/posts") <>) postsPerPage
    k <- getBlogNavbar myPosts
    let v = withJSON k $ extract p
    myBuildPage $(mkRelFile "templates/post-list.html") v out

  phony "index" $
    needIn outputFolder [$(mkRelFile "index.html")]

  phony "post-index" $ do
    xs <- sortedPosts myPosts
    ps <- paginate' postsPerPage xs
    fs <- defaultPagePaths [1..size ps]
    needIn (outputFolder </> $(mkRelDir "posts")) ($(mkRelFile "index.html") : fs)

  phony "by-tag-index" $ do
    xs <- sortedPosts myPosts
    forM_ (viewAllPostTags (snd <$> xs)) $ \t -> do
      u <- parseRelDir $ T.unpack t
      let xs' = filter (elem t . viewTags . snd) xs
      ps <- paginate' postsPerPage xs'
      fs <- defaultPagePaths [1..size ps]
      let tagFolder = outputFolder </> $(mkRelDir "posts/tags") </> u
      needIn tagFolder ($(mkRelFile "index.html") : fs)

  phony "by-month-index" $ do
    xs <- sortedPosts myPosts
    forM_ (viewAllPostTimes (snd <$> xs)) $ \t -> do
      u <- parseRelDir $ defaultMonthUrlFormat t
      let monthFolder = outputFolder </> $(mkRelDir "posts/months") </> u
      let xs' = filter (sameMonth t . viewPostTime . snd) xs
      ps <- paginate' postsPerPage xs'
      fs <- defaultPagePaths [1..size ps]
      needIn monthFolder ($(mkRelFile "index.html") : fs)

  phony "docs" $
    mapM withHtmlExtension tableOfContents >>= needIn outputFolder
    
  phony "posts" $
    getDirectoryFilesWithin' (["posts/*.md"] `within` sourceFolder) >>= 
      mapM (blinkAndMapM outputFolder withHtmlExtension) >>=
        needWithin'

  phony "clean" $ do
    logInfo $ "Cleaning files in " <> display (PathDisplay outputFolder)
    removeFilesAfter outputFolder ["//*"]

tests :: [FilePath] -> TestTree
tests xs = testGroup "Rendering Tests" $
  map ( \x -> goldenVsFile x x
     (replace "golden" "public" x)
     (return ())) xs
  where replace from to' = intercalate to' . splitOn from

main :: IO ()
main = do
   xs <- findByExtension [".html"] "test/golden"
   logOptions' <- logOptionsHandle stdout True
   (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo logOptions')
   shake shakeOptions $ want ["clean"] >> runShakePlus lf rules
   shake shakeOptions $ want ["index", "docs", "posts", "post-index", "by-tag-index", "by-month-index"] >> runShakePlus lf rules
   defaultMain $ tests xs
   dlf
