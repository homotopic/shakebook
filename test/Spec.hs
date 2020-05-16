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

--numPageNeighbours :: Int
--numPageNeighbours = 1

postsPerPage :: Int
postsPerPage = 5

mySocial :: [Value]
mySocial = uncurry genLinkData <$> [("twitter", "http://twitter.com/blanky-site-nowhere")
                                   ,("youtube", "http://youtube.com/blanky-site-nowhere")
                                   ,("gitlab", "http://gitlab.com/blanky-site-nowhere")]
sbc :: SbConfig
sbc = SbConfig {
  sbSrcDir      = sourceFolder
, sbOutDir      = outputFolder
, sbBaseUrl     = baseUrl
, sbMdRead      = defaultMarkdownReaderOptions
, sbHTWrite     = defaultHtml5WriterOptions
, sbPPP         = postsPerPage
, sbGlobalApply = withSiteTitle siteTitle . withHighlighting pygments . withSocialLinks mySocial
}

pagePaths :: MonadThrow m => (Path Rel Dir -> Path Rel File) -> Zipper [] [a] -> m [Path Rel File]
pagePaths f xs = forM [1..size xs] $ parseRelDir . show >=> return . f

rules :: ShakePlus ShakebookEnv ()
rules = view sbConfigL >>= \SbConfig {..} -> do

  readMDC <- newCache loadMarkdownAsJSON 

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

  let myPosts = ["posts/*.md"] `within` sbSrcDir

  ("index.html" `within` sbOutDir) %^> \out -> do
    src <- blinkAndMapM sbSrcDir withMarkdownExtension $ out
    v   <- readMDC src
    r   <- getRecentPosts myPosts
    let v' = withRecentPosts r v
    buildPageActionWithin ($(mkRelFile "templates/index.html") `within` sbSrcDir) v' out

  ("posts/*.html" `within` sbOutDir) %^> \out -> do
    src <- blinkAndMapM sbSrcDir withMarkdownExtension $ out
    xs <- sortedPosts myPosts
    let k = elemIndex src (fst <$> xs)
    let z = fromJust $ liftA2 seek k $ zipper (snd <$> xs)
    r   <- getRecentPosts myPosts
    n   <- getBlogNavbar myPosts
    let v' = withRecentPosts r . withJSON n $ (extract z) 
    buildPageActionWithin ($(mkRelFile "templates/post.html") `within` sbSrcDir) v' out

  toc' <- mapM (mapM withHtmlExtension) $ fmap (`within` sbOutDir) tableOfContents
  void . sequence . flip extend toc' $ \xs -> (fmap toFilePath $ extract xs) %^> \out -> do
    let getDoc = readMDC <=< blinkAndMapM sbSrcDir withMarkdownExtension 
    ys <- mapM getDoc toc'
    zs <- mapM getDoc $ (immediateShoots xs)
    v  <- getDoc $ out
    let v' = withJSON (genTocNavbarData ys) . withSubsections zs $ v
    buildPageActionWithin ($(mkRelFile "templates/docs.html") `within` sbSrcDir) v' out

  ("posts/index.html" `within` sbOutDir) %^>
    copyFileChangedWithin ($(mkRelFile "posts/pages/1/index.html") `within` sbOutDir)

  ("posts/pages/*/index.html" `within` sbOutDir) %^> \out -> do
    xs <- sortedPosts myPosts
    let n = (+ (-1)) . read . (!! 2) . splitOn "/" . toFilePath . extract $ out
    p <- seek n <$> genIndexPageData (snd <$> xs) "Posts" ("/posts/pages" <>) sbPPP
    k <- getBlogNavbar myPosts
    let v = withJSON k $ extract p
    buildPageActionWithin ($(mkRelFile "templates/post-list.html") `within` sbSrcDir) v out
 
  ("posts/tags/*/index.html" `within` sbOutDir) %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/tags/" <> t <> "/pages/1/index.html"
    copyFileChangedWithin (i `within` sbOutDir) out

  ("posts/tags/*/pages/*/index.html" `within` sbOutDir) %^> \out -> do
    let t = T.pack          . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs <- filter (elem t . viewTags . snd) <$> sortedPosts myPosts
    let n = (+ (-1)) . read . (!! 4) . splitOn "/" . toFilePath . extract $ out
    p  <- seek n <$> genIndexPageData (snd <$> xs) ("Posts tagged " <> t) (("/posts/tags/"  <> t <> "/posts") <>) sbPPP
    k <- getBlogNavbar myPosts
    let v = withJSON k $ extract p
    buildPageActionWithin ($(mkRelFile "templates/post-list.html") `within` sbSrcDir) v out

  ("posts/months/*/index.html" `within` sbOutDir) %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/months/" <> t <> "/pages/1/index.html"
    copyFileChangedWithin (i `within` sbOutDir) out

  ("posts/months/*/pages/*/index.html" `within` sbOutDir) %^> \out -> do
    let t = parseISODateTime . T.pack . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs <- filter (sameMonth t . viewPostTime . snd) <$> sortedPosts myPosts
    let n = (+ (-1)) . read  . (!! 4) . splitOn "/" . toFilePath . extract $ out
    p  <- seek n <$> genIndexPageData (snd <$> xs)
                       (("Posts from " <>) . T.pack . defaultPrettyMonthFormat $ t)
                       (("/posts/months/"  <> T.pack (defaultMonthUrlFormat t) <> "/posts") <>) sbPPP
    k <- getBlogNavbar myPosts
    let v = withJSON k $ extract p
    buildPageActionWithin ($(mkRelFile "templates/post-list.html") `within` sbSrcDir) v out

  phony "index" $
    needIn sbOutDir [$(mkRelFile "index.html")]

  phony "post-index" $ do
    xs <- sortedPosts myPosts
    needIn sbOutDir [$(mkRelFile "posts/index.html")]
    paginate' sbPPP xs
      >>= pagePaths (\p -> $(mkRelDir "posts/pages") </> p </> $(mkRelFile "index.html"))
        >>= needIn sbOutDir

  phony "by-tag-index" $ do
    xs <- sortedPosts myPosts
    forM_ (viewAllPostTags (snd <$> xs)) $ \t -> do
      u <- parseRelDir $ T.unpack t
      needIn sbOutDir [$(mkRelDir "posts/tags") </> u </> $(mkRelFile "index.html")]
      paginate' sbPPP xs
        >>= pagePaths (\p -> $(mkRelDir "posts/tags") </> u </> $(mkRelDir "pages") </> p </> $(mkRelFile "index.html"))
          >>= needIn sbOutDir

  phony "by-month-index" $ do
    xs <- sortedPosts myPosts
    forM_ (viewAllPostTimes (snd <$> xs)) $ \t -> do
      u <- parseRelDir $ defaultMonthUrlFormat t
      needIn sbOutDir [$(mkRelDir "posts/months") </> u </> $(mkRelFile "index.html")]
      paginate' sbPPP xs
        >>= pagePaths (\p -> $(mkRelDir "posts/months") </> u </> $(mkRelDir "pages") </> p </> $(mkRelFile "index.html"))
          >>= needIn sbOutDir

  phony "docs" $
    mapM withHtmlExtension tableOfContents >>= needIn sbOutDir
    
  phony "posts" $
    getDirectoryFilesWithin' (["posts/*.md"] `within` sbSrcDir) >>= 
      mapM (blinkAndMapM sbOutDir withHtmlExtension) >>=
        needWithin'

  phony "clean" $ do
    logInfo $ "Cleaning files in " <> display (PathDisplay sbOutDir)
    removeFilesAfter sbOutDir ["//*"]

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
   lf <- newLogFunc (setLogMinLevel LevelInfo logOptions')
   let f = ShakebookEnv (fst lf) sbc
   shake shakeOptions $ want ["clean"] >> runShakePlus f rules
   shake shakeOptions $ want ["index", "docs", "posts", "post-index", "by-tag-index", "by-month-index"] >> runShakePlus f rules--, "docs", "month-index", "posts-index", "tag-index", "posts"]  >> runShakePlus f rules
   defaultMain $ tests xs
   snd lf
