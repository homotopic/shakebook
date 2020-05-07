{-# LANGUAGE TemplateHaskell #-}

import           Control.Comonad.Cofree
import           Control.Comonad.Zipper.Extra
import           Data.Aeson
import           Data.List.Split
import           Development.Shake.Plus
import           Path
import           RIO
import           RIO.List
import qualified RIO.Text                     as T
import           Shakebook
import           Shakebook.Aeson
import           Shakebook.Conventions
import           Shakebook.Defaults
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.Pandoc.Highlighting

sourceFolder :: Path Rel Dir
sourceFolder = $(mkRelDir "test/site")

outputFolder :: Path Rel Dir
outputFolder = $(mkRelDir "test/public")

baseUrl :: Text
baseUrl = "http://blanky.test"

siteTitle :: Text
siteTitle = "Blanky Site"

tableOfContents :: ToC
tableOfContents = "docs/index.md" :< [
                    "docs/1/index.md" :< []
                  , "docs/2/index.md" :< [
                  "docs/2/champ.md" :< []
                  ]
                ]

myBlogNavbar :: [Value] -> Value
myBlogNavbar = genBlogNavbarData "Blog" "/posts/" (T.pack . defaultPrettyMonthFormat) (defaultMonthUrlFragment)

numRecentPosts :: Int
numRecentPosts = 3

numPageNeighbours :: Int
numPageNeighbours = 1

postsPerPage :: Int
postsPerPage = 5

sbc :: SbConfig
sbc = SbConfig {
  sbSrcDir      = sourceFolder
, sbOutDir      = outputFolder
, sbBaseUrl     = baseUrl
, sbMdRead      = defaultMarkdownReaderOptions
, sbHTWrite     = defaultHtml5WriterOptions
, sbPPP         = postsPerPage
, sbGlobalApply = withSiteTitle siteTitle . withHighlighting pygments
}

extendPostsZipper :: MonadShakebookAction r m => Zipper [] Value -> m (Zipper [] Value)
extendPostsZipper = return

enrichPostIndexPage :: MonadShakebookAction r m => [FilePattern] -> Zipper [] Value -> m (Zipper [] Value)
enrichPostIndexPage patterns x = do
  sortedPosts <- loadSortEnrich patterns (Down . viewPostTime) defaultEnrichPost
  return $ fmap (withJSON (myBlogNavbar (snd <$> sortedPosts)))
         . extendPageNeighbours numPageNeighbours $ x

rules :: MonadShakebookRules r m => m ()
rules = do
  defaultSinglePagePattern "index.html"  "templates/index.html"
      (affixRecentPosts ["posts/*.md"] numRecentPosts defaultEnrichPost)

  defaultPostsPatterns     "posts/*.html" "templates/post.html"
      (affixBlogNavbar ["posts/*.md"] "Blog" "/posts/" (T.pack . defaultPrettyMonthFormat) (defaultMonthUrlFragment) defaultEnrichPost
   <=< affixRecentPosts ["posts/*.md"] numRecentPosts defaultEnrichPost
     . defaultEnrichPost)
       extendPostsZipper

  defaultDocsPatterns tableOfContents "templates/docs.html" id

  defaultPostIndexPatterns  ["posts/*.md"] "templates/post-list.html"
                               (enrichPostIndexPage ["posts/*.md"])

  defaultTagIndexPatterns   ["posts/*.md"] "templates/post-list.html"
                               (enrichPostIndexPage ["posts/*.md"])

  defaultMonthIndexPatterns ["posts/*.md"] "templates/post-list.html"
                                (enrichPostIndexPage ["posts/*.md"])
  defaultStaticsPatterns    ["css//*", "images//*", "js//*", "webfonts//*"]
  defaultStaticsPhony       ["css//*", "images//*", "js//*", "webfonts//*"]

  defaultSinglePagePhony    "index" "index.html"
  defaultPostsPhony         ["posts/*.md"]
  defaultPostIndexPhony     ["posts/*.md"]

  defaultTagIndexPhony      ["posts/*.md"]

  defaultMonthIndexPhony    ["posts/*.md"]

  defaultDocsPhony          tableOfContents
  defaultCleanPhony

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
   shake shakeOptions $ want ["index", "docs", "month-index", "posts-index", "tag-index", "posts"]  >> runShakePlus f rules
   defaultMain $ tests xs
   snd lf
