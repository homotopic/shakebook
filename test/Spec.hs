{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

import           Composite.Aeson
import           Composite.Record
import qualified Composite.Record.Tuple          as C
import           Composite.XStep
import qualified Data.IxSet.Typed                as Ix
import qualified Data.IxSet.Typed.Conversions    as Ix
import           Data.List.Split
import           Data.Vinyl                      hiding (RElem)
import           Development.Shake.Plus.Extended
import           Development.Shake.Plus.Forward
import           Path.Extensions
import           RIO
import qualified RIO.ByteString.Lazy             as LBS
import           RIO.List
import qualified RIO.Text                        as T
import           Shakebook                       hiding ((:->))
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.Compdoc
import Path.Utils

-- Config --

sourceDir :: Path Rel Dir
sourceDir = $(mkRelDir "test/site")

outputDir :: Path Rel Dir
outputDir = $(mkRelDir "test/public")

docsDir :: Path Rel Dir
docsDir = $(mkRelDir "docs")

postsDir :: Path Rel Dir
postsDir = $(mkRelDir "posts")

tagsDir :: Path Rel Dir
tagsDir = $(mkRelDir "tags")

monthsDir :: Path Rel Dir
monthsDir = $(mkRelDir "months")

templatesDir :: Path Rel Dir
templatesDir = $(mkRelDir "templates")

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
numPageNeighbours = 3

postsPerPage :: Int
postsPerPage = 5

mySocial :: [Record Link]
mySocial = ["twitter" :*: "http://twitter.com/blanky-site-nowhere" :*: RNil
           ,"youtube" :*: "http://youtube.com/blanky-site-nowhere" :*: RNil
           ,"gitlab"  :*: "http://gitlab.com/blanky-site-nowhere" :*: RNil]

postsRoot :: Text
postsRoot = "/posts/"

tagRoot :: Tag -> Text
tagRoot = (\x -> "/posts/tags/" <> x <> "/") . unTag

monthRoot :: YearMonth -> Text
monthRoot = (\x -> "/posts/months/" <> x <> "/") . defaultMonthUrlFormat . fromYearMonth

enrichment :: Record Enrichment
enrichment = mySocial :*: toHtmlFragment defaultCdnImports :*: toStyleFragment defaultHighlighting :*: siteTitle :*: RNil

-- Loaders

loadMarkdownWith :: (ShakeValue (Record (Compdoc a)), MonadSB r m) => JsonFormat Void (Record a) -> Path Rel File -> m (Record (Compdoc a))
loadMarkdownWith f x = cacheAction ("loader" :: Text, x) $ do
  logInfo $ "Loading " <> displayShow (toFilePath x)
  readMarkdownFile defaultMarkdownReaderOptions defaultHtml5WriterOptions f x

postIndex :: MonadSB r m => Path Rel Dir -> [FilePattern] -> m PostSet
postIndex = batchLoadIndex (\x -> loadMarkdownWith rawPostMetaJsonFormat x >>= prependXStep (stage1PostExtras x))

indexPages :: MonadThrow m => PostSet -> Text -> m (Zipper [] (Record (FUrl : FItems Stage1Post : FPageNo : '[])))
indexPages postIx f = do
  p <- paginate' postsPerPage $ Ix.toDescList (Proxy @Posted) postIx
  return $ p =>> \a -> f <> "pages/" <> T.pack (show $ pos a + 1) :*: extract a :*: pos a + 1 :*: RNil

-- Extras

deriveUrl :: MonadThrow m => Path Rel File -> m Text
deriveUrl = fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceDir

mainPageExtras :: PostSet -> Record (FRecentPosts Stage1Post : '[])
mainPageExtras xs = recentPosts numRecentPosts xs :*: RNil

stage1PostExtras :: (MonadAction m, MonadThrow m) => Path Rel File -> XStep' m RawPost Stage1PostExtras
stage1PostExtras x = pure . view fPosted
                 ::& pure . map (deriveTagLink tagRoot . Tag) . view fTags
                 ::& pure . defaultDeriveTeaser . view fContent
                 ::& deriveUrl . const x
                 ::& XRNil

stage1DocExtras :: MonadThrow m => Path Rel File -> Record RawDoc -> m (Record Stage1DocExtras)
stage1DocExtras x y = fmap C.singleton . deriveUrl $ x

createBlogNav :: PostSet -> Cofree [] (Record Link)
createBlogNav xs = ("Blog" :*: postsRoot :*: RNil)
                :< Ix.toDescCofreeList
                    (C.fanout (defaultPrettyMonthFormat . fromYearMonth) monthRoot)
                    (C.fanout (view fTitle) (view fUrl))
                    (Down . view fPosted)
                    xs

createDocNav :: Cofree [] (Record Stage1Doc) -> Cofree [] (Record Link)
createDocNav = fmap (C.fanout (view fTitle) (view fUrl))

-- Stache Builders

myBuildPage :: (MonadAction m, RMap x, RecordToJsonObject x, RecordFromJson x)
            => PName -> Rec (JsonField e) x -> Record x -> Path Rel File -> m ()
myBuildPage t f x out = do
  k <- compileMustacheDir' t (sourceDir </> templatesDir)
  let l' = renderMustache' k (enrichedRecordJsonFormat f) (enrichment <+> x)
  writeFile' out l'

buildIndex :: MonadSB r m => Record MainPage -> Path Rel File -> m ()
buildIndex = myBuildPage "index" mainPageJsonFields

buildPost :: MonadSB r m => Record FinalPost -> Path Rel File -> m ()
buildPost = myBuildPage "post" finalPostJsonFields

buildDoc :: MonadSB r m => Record FinalDoc -> Path Rel File -> m ()
buildDoc = myBuildPage "docs" finalDocJsonFields

buildPostIndex :: MonadSB r m => Record (IndexPage Stage1Post) -> Path Rel File -> m ()
buildPostIndex = myBuildPage "post-list" postIndexPageJsonFields

-- Rules

docsRules :: MonadSB r m => Path Rel Dir -> Cofree [] (Path Rel File) -> m ()
docsRules dir toc = do
  as <- forM (sourceDir </$> toc) $ \x -> do
    k <- loadMarkdownWith rawDocMetaJsonFormat x
    l <- stage1DocExtras x k
    return $ l <+> k
  let nav = createDocNav as
  sequence_ $ as =>> \(x :< xs) -> do
    out <- fromGroundedUrlF (view fUrl x)
    let v = nav :*: (extract <$> xs) :*: x
    buildDoc v (outputDir </> out)

mainPageRules :: MonadSB r m => m ()
mainPageRules = do
  postIx <- postIndex sourceDir ["posts/*.md"]
  x <- loadMarkdownWith rawSingleMetaJsonFormat $ sourceDir </> $(mkRelFile "index.md")
  let x' = mainPageExtras postIx <+> x
  buildIndex x' $ outputDir </> $(mkRelFile "index.html")


postIndexRules :: MonadSB r m => Cofree [] (Record Link) -> Text -> PostSet -> Text -> m ()
postIndexRules nav title postset root = do
  ys <- indexPages postset root
  sequence_ $ ys =>> \xs' -> do
    let x = extract xs'
    out <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl x)
    ps <- toHtmlFragmentM $ renderPageLinks numPageNeighbours ys
    let x' = ps :*: nav :*: title :*: x
    buildPostIndex x' (outputDir </> out)
  let k = extract $ seek 0 ys
  k' <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl k)
  s' <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD root
  copyFileChanged (outputDir </> k') (outputDir </> s')

postRules :: MonadSB r m => Path Rel Dir -> [FilePattern] -> m PostSet
postRules dir fp = cacheAction ("build" :: T.Text, (dir, fp)) $ do
  postsIx <- postIndex dir fp
  let nav = createBlogNav postsIx
  let postsZ = Ix.toDescList (Proxy @Posted) postsIx
  forM_ postsZ $ \x -> do
    out <- fromGroundedUrlF (view fUrl x)
    let x' = nav :*: x
    buildPost x' (outputDir </> out)
  postIndexRules nav "Posts" postsIx postsRoot
  forM_ (Ix.indexKeys postsIx) $ \t@(Tag t') ->
    postIndexRules nav ("Posts tagged " <> t') (postsIx Ix.@+ [t]) (tagRoot t)
  forM_ (Ix.indexKeys postsIx) \ym@(YearMonth _) ->
    postIndexRules nav ("Posts from " <> defaultPrettyMonthFormat (fromYearMonth ym)) (postsIx Ix.@+ [ym]) (monthRoot ym)
  return postsIx

sitemapRules :: MonadSB r m => PostSet -> Path Rel File -> m ()
sitemapRules xs out = cacheAction ("sitemap" :: T.Text, out) $
  LBS.writeFile (toFilePath $ outputDir </> out) $ renderSitemap $ Sitemap $ asSitemapUrl baseUrl <$> Ix.toList xs

buildRules = do
  mainPageRules
  xs <- postRules sourceDir ["posts/*.md"]
  docsRules sourceDir tableOfContents
  sitemapRules xs $(mkRelFile "sitemap.xml")

tests :: [FilePath] -> TestTree
tests xs = testGroup "Rendering Tests" $
  map (\x -> goldenVsFile x x (replace "golden" "public" x) (return ())) xs where
    replace fr to' = intercalate to' . splitOn fr

main :: IO ()
main = do
  lo <- logOptionsHandle stderr True
  (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo lo)
  let shOpts = shakeOptions { shakeLintInside = ["\\"]}
  shakeArgsForward shOpts lf buildRules
  findByExtension [".html", ".xml"] "test/golden" >>= defaultMain . tests
  dlf
