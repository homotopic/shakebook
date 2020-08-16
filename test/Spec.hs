{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

import Composite.Aeson
import           Composite.Record
import Data.Vinyl hiding (RElem)
import Data.Vinyl.TypeLevel
import qualified Data.IxSet.Typed                as Ix
import qualified Data.IxSet.Typed.Conversions    as Ix
import           Data.List.Split
import           Development.Shake.Plus.Extended
import           Development.Shake.Plus.Forward
import Data.Vinyl.Curry
import           Path.Extensions
import           RIO
import           RIO.List
import Lucid
import           RIO.List.Partial
import qualified RIO.Text                        as T
import qualified RIO.Text.Partial as T
import           Shakebook                       hiding ((:->))
import           Shakebook.Utils
import           Test.Tasty
import           Test.Tasty.Golden
import Composite.Record.Binary()
import Composite.Record.Hashable()
import Development.Shake (ShakeValue)
import Lucid.Base

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

postsPerPage :: Int
postsPerPage = 5

mySocial :: [Record Link]
mySocial = ["twitter" :*: "http://twitter.com/blanky-site-nowhere" :*: RNil
           ,"youtube" :*: "http://youtube.com/blanky-site-nowhere" :*: RNil
           ,"gitlab"  :*: "http://gitlab.com/blanky-site-nowhere" :*: RNil]


type PostSet = Ix.IxSet '[Tag, Posted, YearMonth] (Record Stage1Post)

deriveUrl :: MonadThrow m => Path Rel File -> m Text
deriveUrl = fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceFolder

stage1Post :: (MonadAction m, MonadThrow m) => (Tag -> m Text) -> Record RawPost -> m (Record Stage1Post)
stage1Post f x = do
  u <- deriveUrl $ view fSrcPath x
  k <- mapM (deriveTagLink f . Tag) $ view fTags x
  return $ view fPosted x :*: k :*: deriveTeaser (view fContent x) :*: u :*: x

deriveTeaser :: Text -> Text
deriveTeaser = head . T.splitOn "<!-- more -->"

deriveTagLink :: Monad m => (Tag -> m Text) -> Tag -> m (Record Link)
deriveTagLink f x = rtraverseToSnd (f . Tag) (unTag x)

stage1Doc :: MonadThrow m => Record RawDoc -> m (Record Stage1Doc)
stage1Doc = rtraverseToPush (deriveUrl . view fSrcPath)

enrichment :: Record Enrichment
enrichment = mySocial :*: toHtmlFragment defaultCdnImports :*: toStyleFragment defaultHighlighting :*: siteTitle :*: RNil

type MonadSlick r m = (MonadReader r m, HasLogFunc r, MonadUnliftAction m, MonadThrow m)


type Enrichment = FSocial : FCdnImports : FHighlighting : FSiteTitle : '[]

buildEnrichedPage :: (MonadAction m, MonadThrow m, RMap x, RecordToJsonObject x, RecordFromJson x)
                  => Path b File
                  -> JsonFormatRecord e x
                  -> Record x
                  -> Path b File
                  -> m ()
buildEnrichedPage t f x o = buildPageAction' t (recordJsonFormat $ enrichedXJsonFormatRecord f) (enrichment <+> x) o

-- | given a list of posts this will build a table of contents
buildIndex :: MonadSlick r m => Record MainPage -> Path Rel File -> m ()
buildIndex = buildEnrichedPage (sourceFolder </> $(mkRelFile "templates/index.html")) mainPageJsonFormatRecord

buildPost :: MonadSlick r m => Record FinalPost -> Path Rel File -> m ()
buildPost = buildEnrichedPage (sourceFolder </> $(mkRelFile "templates/post.html")) finalPostJsonFormatRecord

buildDoc :: MonadSlick r m => Record FinalDoc -> Path Rel File -> m ()
buildDoc = buildEnrichedPage (sourceFolder </> $(mkRelFile "templates/docs.html")) finalDocJsonFormatRecord

buildPostIndex :: MonadSlick r m => Record (IndexPage Stage1Post) -> Path Rel File -> m ()
buildPostIndex = buildEnrichedPage (sourceFolder </> $(mkRelFile "templates/post-list.html")) postIndexPageJsonFormatRecord


docsRules :: MonadSlick r m => Path Rel Dir -> Cofree [] (Path Rel File) -> m ()
docsRules dir toc = do
  as <- mapM (loadRawDoc >=> stage1Doc) (fmap (sourceFolder </>) toc)
  nav <- docNavFragment as
  sequence_ $ as =>> \(x :< xs) -> do
    out <- stripProperPrefix sourceFolder =<< replaceExtension ".html" (view fSrcPath x)
    let v = nav :*: (fmap extract $ xs) :*: x
    buildDoc v (outputFolder </> out)

-- | Find and build all posts

loadMarkdownWith :: (ShakeValue a, MonadSlick r m) => JsonFormat Void a -> Path Rel File -> m a
loadMarkdownWith f x = cacheAction ("build" :: Text, x) $ do
  logInfo $ "Loading " <> displayShow (toFilePath x)
  loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions x >>= parseValue' f

loadRawPost :: MonadSlick r m => Path Rel File -> m (Record RawPost)
loadRawPost = loadMarkdownWith rawPostJsonFormat

loadRawSingle :: MonadSlick r m => Path Rel File -> m (Record RawSingle)
loadRawSingle = loadMarkdownWith rawSingleJsonFormat

loadRawDoc :: MonadSlick r m => Path Rel File -> m (Record RawDoc)
loadRawDoc = loadMarkdownWith rawDocJsonFormat

postIndex :: MonadSlick r m => Path Rel Dir -> [FilePattern] -> m PostSet
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
tagRoot= return . toGroundedUrl . (tagsDir </>) <=< parseRelDir . T.unpack . unTag

monthRoot :: MonadThrow m => YearMonth -> m Text
monthRoot = return . toGroundedUrl . (monthsDir </>) <=< parseRelDir . T.unpack . defaultMonthUrlFormat . fromYearMonth

postSetLinks :: MonadThrow m => (YearMonth -> m Text) -> (YearMonth -> m Text) -> PostSet -> m [Cofree [] (Record Link)]
postSetLinks f g xs = forM (Ix.groupDescBy xs) $ \(ym, zs) -> do
  a <- rfanoutM g f ym
  let zs' = sortOn (Down . view fPosted) zs
  let bs = map ((:< []) . rfanout (view fTitle) (view fUrl)) zs'
  return $ a :< bs

type Enriched x = Enrichment ++ x

enrichedXJsonFormatRecord :: JsonFormatRecord e x -> JsonFormatRecord e (Enriched x)
enrichedXJsonFormatRecord x = field (listJsonFormat linkJsonFormat)
                           :& field htmlJsonFormat
                           :& field styleJsonFormat
                           :& field defaultJsonFormat
                           :& x


mainPageExtras :: PostSet -> Record (FRecentPosts Stage1Post : '[])
mainPageExtras xs = recentPosts numRecentPosts xs :*: RNil

mainPageRules :: MonadSlick r m => m ()
mainPageRules = do
  postIx <- postIndex sourceFolder ["posts/*.md"]
  x <- loadRawSingle $ sourceFolder </> $(mkRelFile "index.md")
  let x' = mainPageExtras postIx <+> x
  buildIndex x' $ outputFolder </> $(mkRelFile "index.html")

renderBlogNav :: MonadThrow m => PostSet -> HtmlT m ()
renderBlogNav x = ul_ $ li_ $ do
  renderLink "Blog" "/posts/"
  xs <- lift (postSetLinks monthRoot (return . defaultPrettyMonthFormat . fromYearMonth) x)
  ul_ $ forM_ xs $ li_ . renderCofree (runcurryX renderLink)

renderDocNav :: Monad m => Cofree [] (Record Stage1Doc) -> HtmlT m ()
renderDocNav xs = ul_ $ li_ $ renderCofree (liftA2 renderLink (view fTitle) (view fUrl)) xs

blogNavFragment :: MonadThrow m => PostSet -> m HtmlFragment
blogNavFragment = fmap toHtmlFragment . commuteHtmlT . renderBlogNav

docNavFragment :: MonadThrow m => Cofree [] (Record Stage1Doc) -> m HtmlFragment
docNavFragment = fmap toHtmlFragment . commuteHtmlT . renderDocNav

indexPages :: MonadThrow m => PostSet -> Text -> m (Zipper [] (Record (FUrl : FItems Stage1Post : FPageNo : '[])))
indexPages postIx f = do
  let k = Ix.toDescList (Proxy @Posted) postIx
  p <- paginate' postsPerPage k
  return $ p =>> \a -> f <> "pages/" <> T.pack (show $ pos a + 1) :*: extract a :*: pos a + 1 :*: RNil

pageLinkFragment :: (RElem FPageNo xs, RElem FUrl xs, MonadThrow m) => Zipper [] (Record xs) -> m HtmlFragment
pageLinkFragment = toHtmlFragmentM . renderZipperWithin (liftA2 renderLink (T.pack . show . view fPageNo) (view fUrl)) 3

postRules :: MonadSlick r m => Path Rel Dir -> [FilePattern] -> m ()
postRules dir fp = cacheAction ("build" :: T.Text, (dir, fp)) $ do
  postsIx <- postIndex dir fp
  nav     <- blogNavFragment postsIx
  let postsZ = Ix.toDescList (Proxy @Posted) postsIx
  forM_ postsZ $ \x -> do
    out <- stripProperPrefix sourceFolder =<< replaceExtension ".html" (view fSrcPath x)
    let x' = nav :*: x
    buildPost x' (outputFolder </> out)
  xs <- indexPages postsIx postsRoot
  sequence_ $ xs =>> \xs' -> do
    let x = extract xs'
    out <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl x)
    ps <- pageLinkFragment xs
    let x' = ps :*: nav :*: "Posts" :*: x
    buildPostIndex x' (outputFolder </> out)
  forM_ (Ix.groupDescBy postsIx) \(Tag t, xs') -> do
    ys <- indexPages (postsIx Ix.@+ [Tag t]) =<< tagRoot (Tag t)
    sequence_ $ ys =>> \xs' -> do
      let x = extract xs'
      out <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl x)
      ps <- pageLinkFragment xs'
      let x' = ps :*: nav :*: "Posts tagged" <> t :*: x
      buildPostIndex x' (outputFolder </> out)
  forM_ (Ix.groupDescBy postsIx) \(ym, xs') -> do
    ys <- indexPages (postsIx Ix.@+ [ym]) =<< monthRoot ym
    sequence_ $ ys =>> \xs' -> do
      let x = extract xs'
      out <- (</> $(mkRelFile "index.html")) <$> fromGroundedUrlD (view fUrl x)
      ps <- pageLinkFragment xs'
      let x' = ps :*: nav :*: "Posts from " <> (defaultPrettyMonthFormat $ fromYearMonth ym) :*: x
      buildPostIndex x' (outputFolder </> out)

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
