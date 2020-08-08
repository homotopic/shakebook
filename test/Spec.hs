{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

import Composite.Aeson
import           Composite.Record
import qualified Data.IxSet.Typed                as Ix
import qualified Data.IxSet.Typed.Conversions    as Ix
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake.Plus.Extended
import           Lucid
import           Lucid.Base
import           Path.Extensions
import           RIO
import           RIO.List
import           RIO.List.Partial
import           RIO.Partial
import qualified RIO.Text                        as T
import qualified RIO.Text.Lazy                   as LT
import           Shakebook                       hiding ((:->))
import           Shakebook.Utils
import           Test.Tasty
import           Test.Tasty.Golden
import Data.LanguageCodes
import qualified RIO.Char as C
import Skylighting.Types
import qualified RIO.HashMap as HM

sourceFolder :: Path Rel Dir
sourceFolder = $(mkRelDir "test/site")

outputFolder :: Path Rel Dir
outputFolder = $(mkRelDir "test/public")

baseUrl :: Text
baseUrl = "http://blanky.test"

siteTitle :: Text
siteTitle = "Blanky Site"

tableOfContents :: Cofree [] (Path Rel File)
tableOfContents = $(mkRelFile "docs/index.html") :< [
                    $(mkRelFile "docs/1/index.html") :< []
                  , $(mkRelFile "docs/2/index.html") :< [
                  $(mkRelFile "docs/2/champ.html") :< []
                    ]
                  ]

numRecentPosts :: Int
numRecentPosts = 3

postsPerPage :: Int
postsPerPage = 5

mySocial :: [Record Link]
mySocial = ["twitter" :*: "http://twitter.com/blanky-site-nowhere" :*: RNil
           ,"youtube" :*: "http://youtube.com/blanky-site-nowhere" :*: RNil
           ,"gitlab"  :*: "http://gitlab.com/blanky-site-nowhere" :*: RNil]

addUrl :: (MonadThrow m, RElem FSrcPath xs) => Record xs -> m (Record (FUrl : xs))
addUrl = addDerivedUrl (fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceFolder)

stage1Post :: (MonadAction m, MonadThrow m) => Record RawPost -> m (Record Stage1Post)
stage1Post = addUrl >=> return . addTeaser >=> addTagLinks >=> return . addPrettyDate

stage1Doc :: MonadThrow m => Record RawDoc -> m (Record Stage1Doc)
stage1Doc = addUrl

enrichPage :: Record x -> Record (Enriched x)
enrichPage x = mySocial :*: toHtmlFragment defaultCdnImports :*: toStyleFragment defaultHighlighting :*: siteTitle :*: x

newtype IndexHtml = IndexHtml (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

newtype PostHtml  = PostHtml (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

newtype DocHtml  = DocHtml (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

data PostIndexHtml = PostIndexHtml Text PostsFilter Int (Path Rel Dir, Path Rel File)
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NFData, Binary, Hashable)

type instance RuleResult IndexHtml = Record MainPage
type instance RuleResult PostHtml  = Record FinalPost
type instance RuleResult DocHtml = Record FinalDoc
type instance RuleResult PostIndexHtml = Record PostIndexPage

type instance RuleResult DocSubsections = [Record Stage1Doc]

fromCofree :: (Eq a, Hashable a) => Cofree [] a -> HashMap a [a]
fromCofree = HM.fromList . foldr ((<>) . pure) [] . extend (\(x :< xs) -> (x, fmap extract xs))

newtype DocSubsections = DocSubsections (Path Rel Dir, Path Rel File)
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

newtype DocSubsectionMap a = DocSubsectionMap a
  deriving (Eq, Show, Generic, NFData, Binary, Hashable)

type instance RuleResult (DocSubsectionMap a) = HashMap (Path Rel File) [Path Rel File]

rules :: ShakePlus SimpleSPlusEnv ()
rules = do

  readMD <- newCache $ \x -> do
    logInfo $ "Loading " <> displayShow (toFilePath x)
    loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions x

  readRawSingle <- newCache $ readMD >=> parseValue' rawSingleJsonFormat
  readRawPost   <- newCache $ readMD >=> parseValue' rawPostJsonFormat
  readRawDoc    <- newCache $ readMD >=> parseValue' rawDocJsonFormat

  readStage1Post <- newCache $ readRawPost >=> stage1Post
  readStage1Doc  <- newCache $ readRawDoc  >=> stage1Doc

  postIx' <- newCache $ \() -> batchLoadIndex' (Proxy @[Tag, Posted, YearMonth]) readStage1Post sourceFolder ["posts/*.md"]

  addOracle        defaultIndexRoots
  addOracleCache $ \y -> postIx' () >>= \x -> defaultIndexPages x postsPerPage y

  let correspondingMD   = withMdExtension . (sourceFolder </>)
      getDoc            = correspondingMD >=> readStage1Doc

  addOracleCache $ \(BlogNav ())     -> postIx' () >>= fmap toHtmlFragment . genBlogNav "Blog" defaultPrettyMonthFormat
  addOracleCache $ \(DocNav ())      -> toHtmlFragment . genDocNav <$> mapM getDoc tableOfContents
  addOracleCache $ \(RecentPosts ()) -> take numRecentPosts . Ix.toDescList (Proxy @Posted) <$> postIx' ()
  addOracleCache $ \(IndexHtml (d, o)) -> do
                             v  <- readRawSingle =<< correspondingMD o
                             xs <- askOracle $ RecentPosts ()
                             return $ xs :*: enrichPage v

  addOracleCache $ \(DocSubsectionMap ()) -> return $ fromCofree tableOfContents
  addOracleCache $ \(DocSubsections (d, o)) -> askOracle (DocSubsectionMap ()) >>= mapM getDoc . fromJust . HM.lookup o

  addOracleCache $ \(DocHtml (d, o)) -> do
                             v  <- getDoc o
                             xs <- askOracle $ DocSubsections (d, o)
                             k  <- askOracle $ DocNav ()
                             return $ k :*: xs :*: enrichPage v

  addOracleCache $ \(PostHtml (d, o)) -> do
                      src <- correspondingMD o
                      xs  <- postIx' () >>= Ix.toZipperDesc (Proxy @Posted) >>= seekOnThrow (view fSrcPath) src
                      nav <- askOracle $ BlogNav ()
                      return $ nav :*: enrichPage (extract xs)

  addOracleCache $ \(PostIndexHtml title query pageno (d, o)) -> do
                       nav <- askOracle $ BlogNav ()
                       xs  <- askOracle $ IndexPages query
                       xs' <- zipper' $ sortOn (Down . view fPageNo) xs
                       let links = fmap (\x ->  T.pack (show (view fPageNo x)) :*: view fUrl x :*: RNil) (unzipper xs')
                       return $ enrichPage (links :*: nav :*: title :*: extract (seek (pageno - 1) xs'))

  let buildPage x t f (d, o) = do
       v <- askOracle $ x (d, o)
       buildPageAction (sourceFolder </> t) (toJsonWithFormat f v) (d </> o)

  "index.html"  /%> buildPage IndexHtml $(mkRelFile "templates/index.html") mainPageJsonFormat

  "posts/*.html" /%> buildPage PostHtml $(mkRelFile "templates/post.html") finalPostJsonFormat

  sequence_ $ tableOfContents =>> \xs -> (toFilePath $ extract xs) /%>
    buildPage DocHtml $(mkRelFile "templates/docs.html") finalDocJsonFormat

  "posts/pages/*/index.html" /%> \(dir, fp) -> do
    let n = read . (!! 2) $ splitPath fp
    buildPage (PostIndexHtml "Posts" AllPosts n) $(mkRelFile "templates/post-list.html") postIndexPageJsonFormat (dir, fp)

  "posts/tags/*/pages/*/index.html" /%> \(dir, fp) -> do
    let xs = splitPath fp
    let t  = T.pack $ xs !! 2
    let n  = read $ xs !! 4
    buildPage (PostIndexHtml ("Posts tagged " <> t) (ByTag $ Tag t) n) $(mkRelFile "templates/post-list.html") postIndexPageJsonFormat (dir, fp)

  "posts/months/*/pages/*/index.html" /%> \(dir, fp) -> do
    let xs = splitPath fp
    let t  = parseISODateTime $ T.pack $ xs !! 2
    let t' = YearMonth $ toYearMonthPair t
    let n  = read $ xs !! 4
    buildPage (PostIndexHtml ("Posts from " <> defaultPrettyMonthFormat t) (ByYearMonth t') n) $(mkRelFile "templates/post-list.html") postIndexPageJsonFormat (dir, fp)

  ["posts/index.html", "posts/tags/*/index.html", "posts/months/*/index.html"] /|%> \(dir, fp) -> do
    copyFileChanged (dir </> parent fp </> $(mkRelFile "pages/1/index.html")) (dir </> fp)

  ["css//*", "js//*", "webfonts//*", "images//*"] /|%> \(dir, fp) ->
    copyFileChanged (sourceFolder </> fp) (dir </> fp)

  "sitemap.xml" /%> \(dir, fp) -> do
    xs <- Ix.toDescList (Proxy @Posted) <$> postIx' ()
    buildSitemap (asSitemapUrl baseUrl <$> xs) $ dir </> fp

  let simplePipeline f = getDirectoryFiles sourceFolder >=> mapM f >=> needIn outputFolder
      verbatimPipeline = simplePipeline return

  phony "statics" $ verbatimPipeline ["css//*", "js//*", "webfonts//*", "images//*"]

  phony "index"   $ needIn outputFolder [$(mkRelFile "index.html")]

  phony "docs"    $ mapM withHtmlExtension tableOfContents >>= needIn outputFolder

  phony "posts"   $ simplePipeline withHtmlExtension ["posts/*.md"]

  let phonyIndex x = do
        k  <- askOracle $ IndexRoot x
        ps <- askOracle $ IndexPages x
        xs <- mapM fromGroundedUrlD $ k : (view fUrl <$> ps)
        needIn outputFolder $ fmap (</> $(mkRelFile "index.html")) xs

  phony "post-index" $ do
     phonyIndex AllPosts
     xs <- postIx' ()
     forM_ (Ix.indexKeys xs) $ \t -> phonyIndex $ ByTag t
     forM_ (Ix.indexKeys xs) $ \t -> phonyIndex $ ByYearMonth t

  phony "clean" $ do
    logInfo $ "Cleaning files in " <> displayShow outputFolder
    removeFilesAfter outputFolder ["//*"]

  phony "sitemap" $ needIn outputFolder [$(mkRelFile "sitemap.xml")]

  phony "all" $ need ["index", "posts", "post-index", "docs", "statics", "sitemap"]

tests :: [FilePath] -> TestTree
tests xs = testGroup "Rendering Tests" $
  map (\x -> goldenVsFile x x (replace "golden" "public" x) (return ())) xs where
    replace fr to' = intercalate to' . splitOn fr

main :: IO ()
main = do
   runSimpleShakePlus outputFolder $ want ["clean"] >> rules
   runSimpleShakePlus outputFolder $ want ["all"]   >> rules
   findByExtension [".html", ".xml"] "test/golden" >>= defaultMain . tests
