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
import Shakebook.Data
import Shakebook.Aeson
import Shakebook.Conventions
import Text.Pandoc.Highlighting

srcDir = "test/site"
outDir = "test/public"
baseUrl = "http://blanky.test"

toc = "docs/foo.md" :< [
        "docs/bar.md" :< []
      , "docs/baz.md" :< [
          "docs/quux.md" :< []
        ]
      ]


myBlogNavbar :: [Value] -> Value
myBlogNavbar = genBlogNavbarData "Blog" "/posts/" (T.pack . prettyMonthFormat) (T.pack . monthIndexUrlFormat)

numRecentPosts = 3

numPageNeighbours = 1

sbc = SbConfig srcDir outDir baseUrl markdownReaderOptions html5WriterOptions 5

extendPostsZipper :: Zipper [] Value -> ShakebookA (Zipper [] Value)
extendPostsZipper = return

enrichPostIndexPage :: [FilePattern] -> Zipper [] Value -> ShakebookA (Zipper [] Value)
enrichPostIndexPage patterns x = do
  sortedPosts <- loadSortEnrich patterns (Down . viewPostTime) defaultEnrichPost
  return $ fmap (withJSON (myBlogNavbar (snd <$> sortedPosts)))
         . extendPageNeighbours numPageNeighbours $ x


replace from to = intercalate to . splitOn from

rules = do
  defaultSinglePagePattern "index.html" "templates/index.html"
         (affixRecentPosts ["posts/*.md"] numRecentPosts defaultEnrichPost <=< flap defaultEnrichPost)

  defaultPostsPatterns     "posts/*.html" "templates/post.html"
             (affixBlogNavbar ["posts/*.md"] "Blog" "/posts/" (T.pack . prettyMonthFormat) (T.pack . monthIndexUrlFormat) defaultEnrichPost
         <=< affixRecentPosts ["posts/*.md"] numRecentPosts defaultEnrichPost
         <=< flap defaultEnrichPost . withHighlighting pygments)
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

tests :: [FilePath] -> TestTree
tests xs = testGroup "Rendering Tests" $
  map ( \x ->  (goldenVsFile x x
     (replace "golden" "public" x)
    (shake shakeOptions $ do
      want [replace "golden" "public" x]
      runShakebook sbc rules))) xs

main :: IO ()
main = do
   xs <- findByExtension [".html"] "test/golden"
--   shake shakeOptions $ want ["docs" "month-index", "posts-index", "tag-index", "posts"]  >> runShakebook sbc rules
   defaultMain $ tests xs
