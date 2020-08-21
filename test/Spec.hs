{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

import           Composite.Aeson
import           Composite.Record
import           Composite.Record.Binary         ()
import           Composite.Record.Hashable       ()
import qualified Data.IxSet.Typed                as Ix
import           Data.List.Split
import           Data.Vinyl                      hiding (RElem)
import           Development.Shake               (ShakeValue)
import           Development.Shake.Plus.Extended
import           Development.Shake.Plus.Forward
import           Lucid
import           Path.Extensions
import           RIO
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


deriveUrl :: MonadThrow m => Path Rel File -> m Text
deriveUrl = fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceFolder

stage1Post :: (MonadAction m, MonadThrow m) => (Tag -> m Text) -> Record RawPost -> m (Record Stage1Post)
stage1Post f x = do
  u <- deriveUrl $ view fSrcPath x
  k <- mapM (deriveTagLink f . Tag) $ view fTags x
  return $ view fPosted x :*: k :*: defaultDeriveTeaser (view fContent x) :*: u :*: x

deriveTagLink :: Monad m => (Tag -> m Text) -> Tag -> m (Record Link)
deriveTagLink f x = rtraverseToSnd (f . Tag) (unTag x)

stage1Doc :: MonadThrow m => Record RawDoc -> m (Record Stage1Doc)
stage1Doc = rtraverseToPush (deriveUrl . view fSrcPath)

type Enrichment = FSocialLinks : FCdnImports : FHighlighting : FSiteTitle : '[]

enrichment :: Record Enrichment
enrichment = mySocial :*: toHtmlFragment defaultCdnImports :*: toStyleFragment defaultHighlighting :*: siteTitle :*: RNil


type MonadSB r m = (MonadReader r m, HasLogFunc r, MonadUnliftAction m, MonadThrow m)


myBuildPage :: (MonadAction m, RMap x, RecordToJsonObject x, RecordFromJson x, x <: StandardFields)
            => Path Rel File -> Record x -> Path Rel File -> m ()
myBuildPage t x = buildPageAction' (sourceFolder </> t) (recordJsonFormat (rcast allFields)) (enrichment <+> x)

buildIndex :: MonadSB r m => Record MainPage -> Path Rel File -> m ()
buildIndex = myBuildPage $(mkRelFile "templates/index.html")

buildPost :: MonadSB r m => Record FinalPost -> Path Rel File -> m ()
buildPost = myBuildPage $(mkRelFile "templates/post.html")

buildDoc :: MonadSB r m => Record FinalDoc -> Path Rel File -> m ()
buildDoc = myBuildPage $(mkRelFile "templates/docs.html")

buildPostIndex :: MonadSB r m => Record (IndexPage Stage1Post) -> Path Rel File -> m ()
buildPostIndex = myBuildPage $(mkRelFile "templates/post-list.html")

docsRules :: MonadSB r m => Path Rel Dir -> Cofree [] (Path Rel File) -> m ()
docsRules dir toc = do
  as <- mapM (loadRawDoc >=> stage1Doc) (fmap (sourceFolder </>) toc)
  nav <- toHtmlFragmentM $ renderDocNav as
  sequence_ $ as =>> \(x :< xs) -> do
    out <- stripProperPrefix sourceFolder =<< replaceExtension ".html" (view fSrcPath x)
    let v = nav :*: (extract <$> xs) :*: x
    buildDoc v (outputFolder </> out)

loadMarkdownWith :: (ShakeValue a, MonadSB r m) => JsonFormat Void a -> Path Rel File -> m a
loadMarkdownWith f x = cacheAction ("build" :: Text, x) $ do
  logInfo $ "Loading " <> displayShow (toFilePath x)
  loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions x >>= parseValue' f

loadRawPost :: MonadSB r m => Path Rel File -> m (Record RawPost)
loadRawPost = loadMarkdownWith (recordJsonFormat (rcast basicFields))

loadRawSingle :: MonadSB r m => Path Rel File -> m (Record RawSingle)
loadRawSingle = loadMarkdownWith (recordJsonFormat (rcast basicFields))

loadRawDoc :: MonadSB r m => Path Rel File -> m (Record RawDoc)
loadRawDoc = loadMarkdownWith (recordJsonFormat (rcast basicFields))

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
  renderIxSetGroupDescBy renderMonthLink (liftA2 renderLink (view fTitle) (view fUrl)) (Down . view fPosted) x

renderDocNav :: (Monad m, RElem FTitle xs, RElem FUrl xs) => Cofree [] (Record xs) -> HtmlT m ()
renderDocNav xs = ul_ $ li_ $ renderCofree (liftA2 renderLink (view fTitle) (view fUrl)) xs

indexPages :: MonadThrow m => PostSet -> Text -> m (Zipper [] (Record (FUrl : FItems Stage1Post : FPageNo : '[])))
indexPages postIx f = do
  p <- paginate' postsPerPage $ Ix.toDescList (Proxy @Posted) postIx
  return $ p =>> \a -> f <> "pages/" <> T.pack (show $ pos a + 1) :*: extract a :*: pos a + 1 :*: RNil

renderPageLinks :: (RElem FPageNo xs, RElem FUrl xs, MonadThrow m) => Int -> Zipper [] (Record xs) -> HtmlT m ()
renderPageLinks = renderZipperWithin (liftA2 renderLink (T.pack . show . view fPageNo) (view fUrl))

postIndexRules :: MonadSB r m => Text -> HtmlFragment -> Zipper [] (Record [FUrl, FItems Stage1Post, FPageNo]) -> m ()
postIndexRules title nav ys = do
    sequence_ $ ys =>> \xs' -> do
      let x = extract xs'
      out <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl x)
      ps <- toHtmlFragmentM $ renderPageLinks numPageNeighbours ys
      let x' = ps :*: nav :*: title :*: x
      buildPostIndex x' (outputFolder </> out)

postRules :: MonadSB r m => Path Rel Dir -> [FilePattern] -> m ()
postRules dir fp = cacheAction ("build" :: T.Text, (dir, fp)) $ do
  postsIx <- postIndex dir fp
  nav     <- toHtmlFragmentM $ renderBlogNav postsIx
  let postsZ = Ix.toDescList (Proxy @Posted) postsIx
  forM_ postsZ $ \x -> do
    out <- stripProperPrefix sourceFolder =<< replaceExtension ".html" (view fSrcPath x)
    let x' = nav :*: x
    buildPost x' (outputFolder </> out)
  indexPages postsIx postsRoot >>= postIndexRules "Posts" nav
  forM_ (Ix.indexKeys postsIx) $ \t@(Tag t') ->
    tagRoot t
      >>= indexPages (postsIx Ix.@+ [t])
        >>= postIndexRules ("Posts tagged " <> t') nav
  forM_ (Ix.indexKeys postsIx) \ym@(YearMonth _) ->
    monthRoot ym
      >>= indexPages (postsIx Ix.@+ [ym])
        >>= postIndexRules ("Posts from " <> defaultPrettyMonthFormat (fromYearMonth ym)) nav

buildRules = do
  mainPageRules
  postRules sourceFolder ["posts/*.md"]
  docsRules sourceFolder tableOfContents

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
