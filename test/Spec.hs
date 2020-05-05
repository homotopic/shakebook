{-# LANGUAGE TemplateHaskell #-}
import           Control.Comonad.Cofree
import           Control.Comonad.Store.Zipper
import           Data.Aeson
import           Data.List.Split
import qualified Development.Shake as S
import           Development.Shake (FilePattern)
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

srcDir :: Path Rel Dir
srcDir = $(mkRelDir "test/site")

outDir :: Path Rel Dir
outDir = $(mkRelDir "test/public")

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
myBlogNavbar = genBlogNavbarData "Blog" "/posts/" (T.pack . defaultPrettyMonthFormat) (defaultMonthUrlFragment)

numRecentPosts :: Int
numRecentPosts = 3

numPageNeighbours :: Int
numPageNeighbours = 1

sbc :: SbConfig
sbc = SbConfig srcDir outDir baseUrl defaultMarkdownReaderOptions defaultHtml5WriterOptions 5

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
             (affixBlogNavbar ["posts/*.md"] "Blog" "/posts/" (T.pack . defaultPrettyMonthFormat) (defaultMonthUrlFragment) defaultEnrichPost
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
  defaultStaticsPhony   ["css//*", "images//*", "js//*", "webfonts//*"]

  defaultSinglePagePhony    "index" "index.html"
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
   S.shake S.shakeOptions $ S.want ["clean"] >> runShakePlus f rules
   S.shake S.shakeOptions $ S.want ["index", "docs", "month-index", "posts-index", "tag-index", "posts"]  >> runShakePlus f rules
   defaultMain $ tests xs
   snd lf
