{-# LANGUAGE TemplateHaskell #-}

import           Composite.Aeson
import           Composite.Record
import           Composite.Record.Binary         ()
import           Composite.Record.Hashable       ()
import qualified Data.IxSet.Typed                as Ix
import           Data.List.Split
import           Data.Vinyl                      hiding (RElem)
import           Development.Shake.Plus.Extended
import           Development.Shake.Plus.Forward
import           Lucid
import           Path.Extensions
import           RIO
import qualified RIO.ByteString.Lazy             as LBS
import           RIO.List
import qualified RIO.Text                        as T
import           Shakebook                       hiding ((:->))
import           Shakebook.Utils
import           Test.Tasty
import           Test.Tasty.Golden

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
numPageNeighbours = 3

postsPerPage :: Int
postsPerPage = 5

mySocial :: [Record Link]
mySocial = ["twitter" :*: "http://twitter.com/blanky-site-nowhere" :*: RNil
           ,"youtube" :*: "http://youtube.com/blanky-site-nowhere" :*: RNil
           ,"gitlab"  :*: "http://gitlab.com/blanky-site-nowhere" :*: RNil]

type MonadSB r m = (MonadReader r m, HasLogFunc r, MonadUnliftAction m, MonadThrow m)

loadMarkdownWith :: (ShakeValue a, MonadSB r m) => JsonFormat Void a -> Path Rel File -> m a
loadMarkdownWith f x = cacheAction ("loader" :: Text, x) $ do
  logInfo $ "Loading " <> displayShow (toFilePath x)
  loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions x >>= parseValue' f

loadRawPost :: MonadSB r m => Path Rel File -> m (Record RawPost)
loadRawPost = loadMarkdownWith rawPostJsonFormat

loadRawSingle :: MonadSB r m => Path Rel File -> m (Record RawSingle)
loadRawSingle = loadMarkdownWith rawSingleJsonFormat

loadRawDoc :: MonadSB r m => Path Rel File -> m (Record RawDoc)
loadRawDoc = loadMarkdownWith rawDocJsonFormat

deriveUrl :: MonadThrow m => Path Rel File -> m Text
deriveUrl = fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceFolder

stage1Post :: (MonadAction m, MonadThrow m) => (Tag -> m Text) -> Record RawPost -> m (Record Stage1Post)
stage1Post f x = do
  u <- deriveUrl $ view fSrcPath x
  k <- mapM (deriveTagLink f . Tag) $ view fTags x
  return $ view fPosted x :*: k :*: defaultDeriveTeaser (view fContent x) :*: u :*: x

stage1Doc :: MonadThrow m => Record RawDoc -> m (Record Stage1Doc)
stage1Doc = rtraverseToPush (deriveUrl . view fSrcPath)

enrichment :: Record Enrichment
enrichment = mySocial :*: toHtmlFragment defaultCdnImports :*: toStyleFragment defaultHighlighting :*: siteTitle :*: RNil

myBuildPage :: (MonadAction m, RMap x, RecordToJsonObject x, RecordFromJson x)
            => Path Rel File -> Rec (JsonField e) x -> Record x -> Path Rel File -> m ()
myBuildPage t f x = buildPageAction' (sourceFolder </> t) (enrichedRecordJsonFormat f) (enrichment <+> x)

buildIndex :: MonadSB r m => Record MainPage -> Path Rel File -> m ()
buildIndex = myBuildPage $(mkRelFile "templates/index.html") mainPageJsonFields

buildPost :: MonadSB r m => Record FinalPost -> Path Rel File -> m ()
buildPost = myBuildPage $(mkRelFile "templates/post.html") finalPostJsonFields

buildDoc :: MonadSB r m => Record FinalDoc -> Path Rel File -> m ()
buildDoc = myBuildPage $(mkRelFile "templates/docs.html") finalDocJsonFields

buildPostIndex :: MonadSB r m => Record (IndexPage Stage1Post) -> Path Rel File -> m ()
buildPostIndex = myBuildPage $(mkRelFile "templates/post-list.html") postIndexPageJsonFields

docsRules :: MonadSB r m => Path Rel Dir -> Cofree [] (Path Rel File) -> m ()
docsRules dir toc = do
  as <- mapM (loadRawDoc >=> stage1Doc) (fmap (sourceFolder </>) toc)
  nav <- toHtmlFragmentM $ renderDocNav as
  sequence_ $ as =>> \(x :< xs) -> do
    out <- stripProperPrefix sourceFolder =<< replaceExtension ".html" (view fSrcPath x)
    let v = nav :*: (extract <$> xs) :*: x
    buildDoc v (outputFolder </> out)

postIndex :: MonadSB r m => Path Rel Dir -> [FilePattern] -> m PostSet
postIndex = batchLoadIndex (loadRawPost >=> stage1Post tagRoot)

recentPosts :: Int -> PostSet -> [Record Stage1Post]
recentPosts x y = take x (Ix.toDescList (Proxy @Posted) y)

postsDir  = $(mkRelDir "posts/")
docsDir  = $(mkRelDir "docs/")
tagsDir   = postsDir </> $(mkRelDir "tags/")
monthsDir = postsDir </> $(mkRelDir "months/")

postsRoot :: Text
postsRoot  = toGroundedUrl postsDir

tagRoot :: MonadThrow m => Tag -> m Text
tagRoot = return . toGroundedUrl . (tagsDir </>) <=< parseRelDir . T.unpack . unTag

monthRoot :: MonadThrow m => YearMonth -> m Text
monthRoot = return . toGroundedUrl . (monthsDir </>) <=< parseRelDir . T.unpack . defaultMonthUrlFormat . fromYearMonth

mainPageExtras :: PostSet -> Record (FRecentPosts Stage1Post : '[])
mainPageExtras xs = recentPosts numRecentPosts xs :*: RNil

mainPageRules :: MonadSB r m => m ()
mainPageRules = do
  postIx <- postIndex sourceFolder ["posts/*.md"]
  x <- loadRawSingle $ sourceFolder </> $(mkRelFile "index.md")
  let x' = mainPageExtras postIx <+> x
  buildIndex x' $ outputFolder </> $(mkRelFile "index.html")

renderMonthLink :: MonadThrow m => YearMonth -> HtmlT m ()
renderMonthLink x = do
  u <- lift $ monthRoot x
  renderLink (defaultPrettyMonthFormat . fromYearMonth $ x) u

renderBlogNav :: MonadThrow m => PostSet -> HtmlT m ()
renderBlogNav x = ul_ $ li_ $ do
  renderLink "Blog" "/posts/"
  renderIxSetGroupDescBy renderMonthLink renderTitleLink (Down . view fPosted) x

indexPages :: MonadThrow m => PostSet -> Text -> m (Zipper [] (Record (FUrl : FItems Stage1Post : FPageNo : '[])))
indexPages postIx f = do
  p <- paginate' postsPerPage $ Ix.toDescList (Proxy @Posted) postIx
  return $ p =>> \a -> f <> "pages/" <> T.pack (show $ pos a + 1) :*: extract a :*: pos a + 1 :*: RNil

postIndexRules :: MonadSB r m => HtmlFragment -> Text -> PostSet -> Text -> m ()
postIndexRules nav title postset root = do
    ys <- indexPages postset root
    sequence_ $ ys =>> \xs' -> do
      let x = extract xs'
      out <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl x)
      ps <- toHtmlFragmentM $ renderPageLinks numPageNeighbours ys
      let x' = ps :*: nav :*: title :*: x
      buildPostIndex x' (outputFolder </> out)
    let k = extract $ seek 0 ys
    k' <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl k)
    s' <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD root
    copyFileChanged (outputFolder </> k') (outputFolder </> s')


postRules :: MonadSB r m => Path Rel Dir -> [FilePattern] -> m PostSet
postRules dir fp = cacheAction ("build" :: T.Text, (dir, fp)) $ do
  postsIx <- postIndex dir fp
  nav     <- toHtmlFragmentM $ renderBlogNav postsIx
  let postsZ = Ix.toDescList (Proxy @Posted) postsIx
  forM_ postsZ $ \x -> do
    out <- stripProperPrefix sourceFolder =<< replaceExtension ".html" (view fSrcPath x)
    let x' = nav :*: x
    buildPost x' (outputFolder </> out)
  postIndexRules nav "Posts" postsIx postsRoot
  forM_ (Ix.indexKeys postsIx) $ \t@(Tag t') ->
    tagRoot t >>= postIndexRules nav ("Posts tagged " <> t') (postsIx Ix.@+ [t])
  forM_ (Ix.indexKeys postsIx) \ym@(YearMonth _) ->
    monthRoot ym >>= postIndexRules nav ("Posts from " <> defaultPrettyMonthFormat (fromYearMonth ym)) (postsIx Ix.@+ [ym])
  return postsIx

sitemapRules :: MonadSB r m => PostSet -> Path Rel File -> m ()
sitemapRules xs out = cacheAction ("sitemap" :: T.Text, out) $
  LBS.writeFile (toFilePath $ outputFolder </> out) $ renderSitemap $ Sitemap $ fmap (asSitemapUrl baseUrl) $ Ix.toList xs

buildRules = do
  mainPageRules
  xs <- postRules sourceFolder ["posts/*.md"]
  docsRules sourceFolder tableOfContents
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
