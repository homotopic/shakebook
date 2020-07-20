{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

import           Composite.Record
import qualified Data.IxSet.Typed             as Ix
import qualified Data.IxSet.Typed.Conversions as Ix
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake.Plus.Extended
import           Lucid
import           Path.Extensions
import           RIO
import           RIO.List
import           RIO.List.Partial
import           RIO.Partial
import qualified RIO.Text                     as T
import qualified RIO.Text.Partial             as T
import           Shakebook                    hiding ((:->))
import           Test.Tasty
import           Test.Tasty.Golden
import           Within

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

myBlogNav :: (Ix.IsIndexOf YearMonth ixs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs)
          => Ix.IxSet ixs (Record xs)
          -> Html ()
myBlogNav = genBlogNavbarData "Blog" "/posts/" defaultPrettyMonthFormat defaultMonthUrlFragment

myDocNav :: (RElem FUrl xs, RElem FTitle xs) => Cofree [] (Record xs) -> Html ()
myDocNav = genTocNavbarData

addUrl :: (MonadThrow m, RElem FSrcPath xs) => Record xs -> m (Record (FUrl : xs))
addUrl = addDerivedUrl (fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceFolder)

addTagLinks :: RElem FTags xs => Record xs -> Record (FTagLinks : xs)
addTagLinks xs = (fmap (\x -> x :*: ("/posts/tags/" <> x) :*: RNil) . viewTags $ xs ) :*: xs

addTeaser :: RElem FContent xs => Record xs -> Record (FTeaser : xs)
addTeaser xs = head (T.splitOn "<!-- more -->" (viewContent xs)) :*: xs

addPrettyDate :: RElem FPosted xs => Record xs -> Record (FPrettyDate : xs)
addPrettyDate xs = viewPosted xs :*: xs

stage1Post :: (MonadAction m, MonadThrow m) => Record RawPost -> m (Record Stage1Post)
stage1Post = addUrl >=> return . addPrettyDate . addTagLinks . addTeaser

stage1Doc :: MonadThrow m => Record RawDoc -> m (Record Stage1Doc)
stage1Doc = addUrl

enrichPage :: Record x -> Record (Enriched x)
enrichPage x = mySocial :*: defaultCdnImports :*: defaultHighlighting :*: siteTitle :*: x

data PostIndex k a where
  AllPosts    :: PostIndex k (PostSet k)
  ByTag       :: Tag -> PostIndex k (PostSet k)
  ByYearMonth :: YearMonth -> PostIndex k (PostSet k)
  DescPosted  :: PostIndex k (PostSet k) -> PostIndex k [Record k]
  DescPostedZ :: PostIndex k (PostSet k) -> PostIndex k (Zipper [] (Record k))
  RecentPosts :: Int -> PostIndex k [Record k]
  Paginate    :: Int -> PostIndex k [Record k] -> PostIndex k (Zipper [] [Record k])
  PagesRoot   :: PostIndex k (PostSet k) -> PostIndex k Text
  PagesLinks  :: Int -> PostIndex k (PostSet k) -> PostIndex k (Zipper [] (Record Link))

postIndex :: (Ix.Indexable '[Tag, Posted, YearMonth] (Record k), MonadAction m, MonadThrow m)
          => Ix.IxSet '[Tag, Posted, YearMonth] (Record k)
          -> PostIndex k a
          -> m a
postIndex rd AllPosts        = return rd
postIndex rd (ByTag t)       = (Ix.@+ [t]) <$> postIndex rd AllPosts
postIndex rd (ByYearMonth t) = (Ix.@+ [t]) <$> postIndex rd AllPosts
postIndex rd (DescPosted x)  = Ix.toDescList (Proxy @Posted) <$> postIndex rd x
postIndex rd (DescPostedZ x) = Ix.toZipperDesc (Proxy @Posted) =<< postIndex rd x
postIndex rd (RecentPosts x) = take x <$> postIndex rd (DescPosted AllPosts)
postIndex rd (Paginate x f)  = postIndex rd f >>= paginate' x
postIndex _  (PagesRoot f)   = return $ case f of
      AllPosts      -> "/posts/"
      ByTag (Tag t) -> "/posts/tags/" <> t <> "/"
      ByYearMonth (YearMonth (y, m)) -> "/posts/months/" <> defaultMonthUrlFormat (fromYearMonthPair (y, m)) <> "/"
postIndex rd (PagesLinks n f) = do
  xs <- postIndex rd (Paginate n (DescPosted f))
  u <- postIndex rd (PagesRoot f)
  return $ extend (\x -> T.pack (show (pos x + 1)) :*: (u <> "pages/" <> T.pack (show (pos x + 1))) :*: RNil) xs


rules :: ShakePlus LogFunc ()
rules = do

  readMD <- newCache $ \x -> do
    logInfo $ "Loading " <> displayShow (toFilePath x)
    loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions x

  readRawSingle <- newCache $ readMD >=> parseValue' rawSingleJsonFormat
  readRawPost   <- newCache $ readMD >=> parseValue' rawPostJsonFormat
  readRawDoc    <- newCache $ readMD >=> parseValue' rawDocJsonFormat

  readStage1Post <- newCache $ readRawPost >=> stage1Post
  readStage1Doc  <- newCache $ readRawDoc  >=> stage1Doc

  postIx' <- newCache $ \() -> batchLoadIndex readStage1Post sourceFolder ["posts/*.md"]

  let o' = (`within` outputFolder)

      blogNav = myBlogNav <$> postIx AllPosts
      postIx :: PostIndex Stage1Post a -> RAction LogFunc a
      postIx l = postIx' () >>= \k -> postIndex k l
      indexHtml = $(mkRelFile "index.html") :: Path Rel File

  o' "index.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    v   <- readRawSingle $ fromWithin src
    xs  <- postIx $ RecentPosts numRecentPosts
    let (v' :: TMain) = Val $ xs :*: enrichPage v
    buildPageAction' sourceFolder v' mainPageJsonFormat $ fromWithin out

  o' "posts/*.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    xs  <- postIx (DescPostedZ AllPosts) >>= seekOnThrow viewSrcPath (fromWithin src)
    nav <- blogNav
    let (v :: TPost) = Val $ nav :*: enrichPage (extract xs)
    buildPageAction' sourceFolder v finalPostJsonFormat $ fromWithin out

  toc' <- mapM (mapM withHtmlExtension) $ fmap o' tableOfContents
  sequence_ $ toc' =>> \xs -> (toFilePath <$> extract xs) %^> \out -> do
    let getDoc = readStage1Doc . fromWithin <=< blinkAndMapM sourceFolder withMdExtension
    ys <- mapM getDoc toc'
    zs <- mapM getDoc (fmap extract . unwrap $ xs)
    v  <- getDoc out
    let (v' :: TDoc) = Val $ myDocNav ys :*: zs :*: enrichPage v
    buildPageAction' sourceFolder v' finalDocJsonFormat $ fromWithin out

  o' "posts/index.html" %^> \out -> 
    copyFileChanged (outputFolder </> $(mkRelFile "posts/pages/1/index.html")) $ fromWithin out

  o' "posts/pages/*/index.html" %^> \out -> do
    let n = read . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs  <- postIx $ Paginate   postsPerPage $ DescPosted AllPosts
    ys  <- postIx $ PagesLinks postsPerPage   AllPosts
    nav <- blogNav
    let (v :: TPostIndex) = Val $ enrichPage (unzipper ys :*: nav :*: extract (seek (n -1) xs) :*: "Posts" :*: RNil)
    buildPageAction' sourceFolder v postIndexPageJsonFormat $ fromWithin out

  o' "posts/tags/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/tags/" <> t <> "/pages/1/index.html"
    copyFileChanged (outputFolder </> i) (fromWithin out)

  o' "posts/tags/*/pages/*/index.html" %^> \out -> do
    let zs = splitOn "/" . toFilePath . extract $ out
    let t = T.pack $ zs !! 2
    let n = read   $ zs !! 4
    nav <- blogNav
    xs  <- postIx $ Paginate   postsPerPage $ DescPosted $ ByTag $ Tag t
    ys  <- postIx $ PagesLinks postsPerPage $ ByTag $ Tag t
    let (v :: TPostIndex) = Val $ enrichPage (unzipper ys :*: nav :*: extract (seek (n -1) xs) :*: "Posts tagged " <> t :*: RNil)
    buildPageAction' sourceFolder v postIndexPageJsonFormat $ fromWithin out

  o' "posts/months/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/months/" <> t <> "/pages/1/index.html"
    copyFileChanged (outputFolder </> i) (fromWithin out)

  o' "posts/months/*/pages/*/index.html" %^> \out -> do
    let zs = splitOn "/" . toFilePath . extract $ out
    let t = parseISODateTime $ T.pack $ zs !! 2
    let t' = YearMonth $ toYearMonthPair t
    let n = read $ zs !! 4
    nav <- blogNav
    xs  <- postIx $ Paginate   postsPerPage $ DescPosted $ ByYearMonth t'
    ys  <- postIx $ PagesLinks postsPerPage $ ByYearMonth t'
    let (v :: TPostIndex) = Val $ enrichPage (unzipper ys :*: nav :*: extract (seek (n -1) xs) :*: "Posts from " <> defaultPrettyMonthFormat t :*: RNil)
    buildPageAction' sourceFolder v postIndexPageJsonFormat $ fromWithin out

  o' ["css//*", "js//*", "webfonts//*", "images//*"] |%^> \out ->
    copyFileChanged (fromWithin $ blinkLocalDir sourceFolder out) $ fromWithin out

  o' "sitemap.xml" %^> \out -> do
    xs <- postIx $ DescPosted AllPosts
    buildSitemap (asSitemapUrl baseUrl <$> xs) $ fromWithin out

  let simplePipeline f = getDirectoryFiles sourceFolder >=> mapM f >=> needIn outputFolder
      verbatimPipeline = simplePipeline return

  phony "statics" $ verbatimPipeline ["css//*", "js//*", "webfonts//*", "images//*"]

  phony "index" $ needIn outputFolder [indexHtml]

  phony "post-index" $ do
     k  <- postIx (PagesRoot AllPosts) >>= fromGroundedUrlD
     ps <- postIx (PagesLinks postsPerPage AllPosts) >>= mapM (fromGroundedUrlD . viewUrl)
     needIn outputFolder (fmap (</> indexHtml) ps)
     needIn (outputFolder </> k) [indexHtml]

  phony "by-tag-index" $ do
     xs <- postIx AllPosts
     forM_ (Ix.indexKeys xs) \t@(Tag _) -> do
       k  <- postIx (PagesRoot $ ByTag t) >>= fromGroundedUrlD
       ps <- postIx (PagesLinks postsPerPage $ ByTag t) >>= mapM (fromGroundedUrlD . viewUrl)
       needIn outputFolder (fmap (</> indexHtml) ps)
       needIn (outputFolder </> k) [indexHtml]

  phony "by-month-index" $ do
     xs <- postIx AllPosts
     forM_ (Ix.indexKeys xs) \t@(YearMonth _) -> do
       k  <- postIx (PagesRoot (ByYearMonth t)) >>= fromGroundedUrlD
       ps <- postIx (PagesLinks postsPerPage (ByYearMonth t)) >>= mapM (fromGroundedUrlD . viewUrl)
       needIn outputFolder (fmap (</> indexHtml) ps)
       needIn (outputFolder </> k) [indexHtml]

  phony "docs" $
    mapM withHtmlExtension tableOfContents >>= needIn outputFolder

  phony "posts" $ simplePipeline withHtmlExtension ["posts/*.md"]

  phony "clean" $ do
    logInfo $ "Cleaning files in " <> displayShow outputFolder
    removeFilesAfter outputFolder ["//*"]

  phony "sitemap" $ needIn outputFolder [$(mkRelFile "sitemap.xml") :: Path Rel File]

tests :: [FilePath] -> TestTree
tests xs = testGroup "Rendering Tests" $
  map ( \x -> goldenVsFile x x
     (replace "golden" "public" x)
     (return ())) xs
  where replace fr to' = intercalate to' . splitOn fr

main :: IO ()
main = do
   xs <- findByExtension [".html", ".xml"] "test/golden"
   logOptions' <- logOptionsHandle stdout True
   (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo logOptions')
   shake shakeOptions $ want ["clean"] >> runShakePlus lf rules
   shake shakeOptions $ want ["index", "docs", "posts", "post-index", "by-tag-index", "by-month-index", "sitemap"] >> runShakePlus lf rules
  -- shake shakeOptions $ want ["index", "docs", "posts", "post-index"] >> runShakePlus lf rules
   defaultMain $ tests xs
   dlf
