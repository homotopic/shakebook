{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Comonad.Cofree
import Development.Shake
import RIO
import RIO.List
import Data.List.Split
import qualified RIO.Text as T
import Shakebook
import Shakebook.Defaults
import Test.Tasty
import Test.Tasty.Golden
import Data.Aeson
import Control.Comonad.Store.Zipper
import Shakebook.Aeson
import Shakebook.Conventions
import Text.Pandoc.Highlighting

srcDir :: String
srcDir = "test/site"

outDir :: String
outDir = "test/public"

baseUrl :: Text
baseUrl = "http://blanky.test"

toc :: ToC
toc = "docs/index.md" :< [
        "docs/1/index.md" :< []
      , "docs/2/index.md" :< [
          "docs/2/champ.md" :< []
        ]
      ]

myBlogNavbar :: [Value] -> Value
myBlogNavbar = genBlogNavbarData "Blog" "/posts/" (T.pack . prettyMonthFormat) (T.pack . monthIndexUrlFormat)

numRecentPosts :: Int
numRecentPosts = 3

numPageNeighbours :: Int
numPageNeighbours = 1

sbc :: SbConfig
sbc = SbConfig srcDir outDir baseUrl markdownReaderOptions html5WriterOptions 5

extendPostsZipper :: MonadShakebookAction r m => Zipper [] Value -> m (Zipper [] Value)
extendPostsZipper = return

enrichPostIndexPage :: MonadShakebookAction r m => [FilePattern] -> Zipper [] Value -> m (Zipper [] Value)
enrichPostIndexPage patterns x = do
  sortedPosts <- loadSortEnrich patterns (Down . viewPostTime) defaultEnrichPost
  return $ fmap (withJSON (myBlogNavbar (snd <$> sortedPosts)))
         . extendPageNeighbours numPageNeighbours $ x

rules :: MonadShakebookRules r m => m ()
rules = do
  defaultSinglePagePattern "index.html" "templates/index.html"
         (affixRecentPosts ["posts/*.md"] numRecentPosts defaultEnrichPost)

  defaultPostsPatterns     "posts/*.html" "templates/post.html"
             (affixBlogNavbar ["posts/*.md"] "Blog" "/posts/" (T.pack . prettyMonthFormat) (T.pack . monthIndexUrlFormat) defaultEnrichPost
         <=< affixRecentPosts ["posts/*.md"] numRecentPosts defaultEnrichPost
         . defaultEnrichPost . withHighlighting pygments)
              extendPostsZipper

  defaultDocsPatterns      toc "templates/docs.html"
                               (withHighlighting pygments)

  defaultPostIndexPatterns ["posts/*.md"] "templates/post-list.html"
                               (enrichPostIndexPage ["posts/*.md"])

  defaultTagIndexPatterns  ["posts/*.md"] "templates/post-list.html"
                               (enrichPostIndexPage ["posts/*.md"])

  defaultMonthIndexPatterns ["posts/*.md"] "templates/post-list.html"
                                (enrichPostIndexPage ["posts/*.md"])
  defaultStaticsPatterns   ["css//*", "images//*", "js//*", "webfonts//*"]
  defaultStaticsPhony

  defaultPostsPhony         ["posts/*.md"]
  defaultPostIndexPhony     ["posts/*.md"]

  defaultTagIndexPhony      ["posts/*.md"]

  defaultMonthIndexPhony    ["posts/*.md"]

  defaultDocsPhony          toc
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
   shake shakeOptions $ want ["clean"] >> runShakebook f rules
   shake shakeOptions $ want ["docs", "month-index", "posts-index", "tag-index", "posts"]  >> runShakebook f rules
   defaultMain $ tests xs
   snd lf


