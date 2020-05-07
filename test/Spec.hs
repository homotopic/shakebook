{-# LANGUAGE TemplateHaskell #-}

import           Control.Comonad.Cofree
import           Data.Aeson
import           Data.List.Split
import           Development.Shake.Plus
import           Path
import           RIO
import           RIO.List
import qualified RIO.Text                     as T
import           Shakebook
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

myRecentPosts :: MonadShakebookAction r m => Value -> m Value
myRecentPosts = affixRecentPosts ["posts/*.md"] numRecentPosts defaultEnrichPost

myBlogNavbar :: MonadShakebookAction r m => Value -> m Value
myBlogNavbar = affixBlogNavbar ["posts/*.md"] "Blog" "/posts/"
                  (T.pack . defaultPrettyMonthFormat)
                  (defaultMonthUrlFragment)
                  defaultEnrichPost

rules :: MonadShakebookRules r m => m ()
rules = do
  defaultSinglePagePattern "index.html"  "templates/index.html" myRecentPosts

  defaultPostsPatterns     "posts/*.html" "templates/post.html"
    (myBlogNavbar <=< myRecentPosts . defaultEnrichPost) pure

  defaultDocsPatterns tableOfContents "templates/docs.html" id

  defaultPostIndexPatterns  ["posts/*.md"] "templates/post-list.html" myBlogNavbar
                               (pure . extendPageNeighbours numPageNeighbours)

  defaultTagIndexPatterns   ["posts/*.md"] "templates/post-list.html" myBlogNavbar
                               (pure . extendPageNeighbours numPageNeighbours)

  defaultMonthIndexPatterns ["posts/*.md"] "templates/post-list.html" myBlogNavbar
                               (pure . extendPageNeighbours numPageNeighbours)

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
