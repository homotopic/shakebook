{-# LANGUAGE TemplateHaskell #-}

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Comonad.Zipper.Extra
import           Data.Aeson
import           Data.List.Split
import           Development.Shake.Plus
import           Path
import           RIO
import           RIO.Partial
import qualified RIO.HashMap                  as HM
import           RIO.List
import qualified RIO.Text                     as T
import           Shakebook.Aeson
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

numPageNeighbours :: Int
numPageNeighbours = 1

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

rules :: MonadShakebookRules r m => m ()
rules = view sbConfigL >>= \SbConfig {..} -> do

  readMDC <- newCache readMarkdownFile'

  postsC  <- newCache $ \w -> do
    xs <- batchLoadWithin' w readMDC
    return $ defaultEnrichPost <$> xs

  getRecentPosts <- newCache $ \fp -> do
     allPosts <- postsC fp
     return $ take numRecentPosts (sortOn (Down . viewPostTime) $ HM.elems allPosts)

  getBlogNavbar <- newCache $ \fp -> do
     allPosts <- postsC fp
     return $ genBlogNavbarData "Blog" "/posts/"
                  (T.pack . defaultPrettyMonthFormat)
                  (defaultMonthUrlFragment) (HM.elems allPosts)

  ("index.html" `within` sbOutDir) %^> \out -> do
    src <- blinkAndMapM sbSrcDir withMarkdownExtension $ out
    v   <- readMDC src
    r   <- getRecentPosts (["posts/*.md"] `within` sbSrcDir)
    let v' = withRecentPosts r v
    buildPageActionWithin ($(mkRelFile "templates/index.html") `within` sbSrcDir) v' out

  ("posts/*.html" `within` sbOutDir) %^> \out -> do
    src <- blinkAndMapM sbSrcDir withMarkdownExtension $ out
    allPosts <- postsC    (["posts/*.md"] `within` sbSrcDir)
    let sortedPosts = sortOn (Down . viewPostTime . snd) $ HM.toList allPosts
    let k = elemIndex src (fst <$> sortedPosts)
    let z = fromJust $ liftA2 seek k $ zipper (snd <$> sortedPosts)
    r   <- getRecentPosts (["posts/*.md"] `within` sbSrcDir)
    n   <- getBlogNavbar  (["posts/*.md"] `within` sbSrcDir)
    let v' = withRecentPosts r . withJSON n $ (extract z) 
    buildPageActionWithin ($(mkRelFile "templates/post.html") `within` sbSrcDir) v' out

  toc' <- mapM (mapM withHtmlExtension) $ fmap (`within` sbOutDir) tableOfContents
  sequence . flip extend toc' $ \xs -> (fmap toFilePath $ extract xs) %^> \out -> do
    let getDoc = readMDC <=< blinkAndMapM sbSrcDir withMarkdownExtension 
    ys <- mapM getDoc toc'
    zs <- mapM getDoc $ (Shakebook.Data.lower xs)
    v  <- getDoc $ out
    let v' = withJSON (genTocNavbarData ys) . withSubsections zs $ v
    buildPageActionWithin ($(mkRelFile "templates/docs.html") `within` sbSrcDir) v' out

--  storeRuleGenWithin ("posts/index.html" `within` sbOutDir) (const 0) (const ()) 

  phony "index" $
    needIn sbOutDir [$(mkRelFile "index.html")]

  phony "post-index" $ do
    xs <-getDirectoryFilesWithin' (["posts/*.md"] `within` sbSrcDir) >>= mapM readMDC
    needIn sbOutDir [$(mkRelFile "posts/index.html")]
    paginate' sbPPP xs
      >>= pagePaths (\p -> $(mkRelDir "posts") </> p </> $(mkRelFile "index.html"))
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
   shake shakeOptions $ want ["index", "docs", "posts"] >> runShakePlus f rules--, "docs", "month-index", "posts-index", "tag-index", "posts"]  >> runShakePlus f rules
   defaultMain $ tests xs
   snd lf
