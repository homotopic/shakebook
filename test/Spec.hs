{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BlockArguments #-}

import Data.Aeson as Aeson
import Data.Aeson.BetterErrors as ABE
import qualified Data.IxSet.Typed as Ix
import qualified Data.IxSet.Typed.Conversions as Ix
import           Data.List.Split
import           Data.Text.Time
import           Path.Extensions
import           Lucid
import           RIO
import qualified RIO.HashMap       as HM
import           RIO.List
import           RIO.List.Partial
import           RIO.Partial
import qualified RIO.Text          as T
import qualified RIO.Text.Lazy          as LT
import qualified RIO.Text.Partial as T
import           Shakebook hiding ((:->))
import Composite.Aeson
import Composite.Record
import           Test.Tasty
import           Test.Tasty.Golden
import RIO.Time

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
numPageNeighbours = 1

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

--- Stage 0 Types

-- "Basic Markdown" - These two fields are always populated by the markdown loader - the source path and the main body content.
type BasicMD = FSrcPath : FContent : '[]

basicMDJsonFormat :: JsonFormatRecord e BasicMD
basicMDJsonFormat = field relFileJsonFormat
                 :& field textJsonFormat
                 :& RNil

-- A 'RawDoc' contains three mandatory fields - title, description and modified.
type RawDoc = FDescription : FTitle : FModified : BasicMD

rawDocJsonFormat :: JsonFormatRecord e RawDoc
rawDocJsonFormat = field aesonJsonFormat
                :& field defaultJsonFormat
                :& field iso8601DateTimeJsonFormat
                :& basicMDJsonFormat

-- A `RawPost` contains three mandatory fields, title, tags, and posted, and an optional image field.
type RawPost = FTitle : FImage : FTags : FPosted : BasicMD

rawPostJsonFormat :: JsonFormatRecord e RawPost
rawPostJsonFormat = field textJsonFormat
                 :& optionalField textJsonFormat
                 :& field (listJsonFormat textJsonFormat)
                 :& field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%F" "yyyy-mm-dd" :| []))
                 :& basicMDJsonFormat

type RawSingle = FTitle : FImage : BasicMD

rawSingleJsonFormat :: JsonFormatRecord e RawSingle
rawSingleJsonFormat = field textJsonFormat
                   :& optionalField textJsonFormat
                   :& basicMDJsonFormat

type URLised x = FUrl : x

urlisedXJsonFormat :: JsonFormatRecord e x -> JsonFormatRecord e (URLised x)
urlisedXJsonFormat x = field defaultJsonFormat :& x

addUrl :: (MonadThrow m, RElem FSrcPath xs) => Record xs -> m (Record (FUrl : xs))
addUrl = addDerivedUrl (fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceFolder)

type Stage1Post = FPrettyDate : FTagLinks : FTeaser : URLised RawPost

stage1PostJsonFormat :: JsonFormatRecord e Stage1Post
stage1PostJsonFormat = field (dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%A, %B %d, %Y" "yyyy-mm-dd" :| []))
                    :& field (listJsonFormat $ recordJsonFormat linkJsonFormat)
                    :& field textJsonFormat
                    :& urlisedXJsonFormat rawPostJsonFormat

addTagLinks :: RElem FTags xs => Record xs -> Record (FTagLinks : xs)
addTagLinks xs = (fmap (\x -> x :*: ("/posts/tags/" <> x) :*: RNil) . viewTags $ xs ) :*: xs

addTeaser :: RElem FContent xs => Record xs -> Record (FTeaser : xs)
addTeaser xs = head (T.splitOn "<!-- more -->" (viewContent xs)) :*: xs

addPrettyDate :: RElem FPosted xs => Record xs -> Record (FPrettyDate : xs)
addPrettyDate xs = viewPosted xs :*: xs

stage1Post :: (MonadAction m, MonadThrow m) => Record RawPost -> m (Record Stage1Post)
stage1Post = addUrl >=> return . addPrettyDate . addTagLinks . addTeaser

type Stage1Doc = URLised RawDoc

stage1DocJsonFormat :: JsonFormatRecord e Stage1Doc
stage1DocJsonFormat = urlisedXJsonFormat rawDocJsonFormat

stage1Doc :: (MonadAction m, MonadThrow m) => Record RawDoc -> m (Record Stage1Doc)
stage1Doc = addUrl

--- Stage 2 Types

-- Enrichment provides fields most page templates require.
type Enriched x = FSocial : FCdnImports : FHighlighting : FSiteTitle : x

linkJsonFormat :: JsonFormatRecord e Link
linkJsonFormat = field textJsonFormat :& field textJsonFormat :& RNil

enrichedXJsonFormat :: JsonFormatRecord e x -> JsonFormatRecord e (Enriched x)
enrichedXJsonFormat x = field (listJsonFormat (recordJsonFormat linkJsonFormat))
                     :& field lucidJsonFormat
                     :& field styleJsonFormat
                     :& field defaultJsonFormat
                     :& x

enrichPage :: Record x -> Record (Enriched x)
enrichPage x = mySocial :*: defaultCdnImports :*: defaultHighlighting :*: siteTitle :*: x

type FinalDoc = FToc : FSubsections Stage1Doc : Enriched Stage1Doc

finalDocJsonFormat :: JsonFormatRecord e FinalDoc
finalDocJsonFormat = field lucidJsonFormat
                  :& field (listJsonFormat $ recordJsonFormat stage1DocJsonFormat)
                  :& enrichedXJsonFormat stage1DocJsonFormat

type FinalPost = FToc : Enriched Stage1Post

finalPostJsonFormat :: JsonFormatRecord e FinalPost
finalPostJsonFormat = field lucidJsonFormat
                   :& enrichedXJsonFormat stage1PostJsonFormat

type IndexPage x = Enriched (FPageLinks : FToc : FItems x : FTitle : '[])

indexPageJsonFormat :: JsonFormat e (Record x) -> JsonFormatRecord e (IndexPage x)
indexPageJsonFormat x = enrichedXJsonFormat $ field (listJsonFormat $ recordJsonFormat linkJsonFormat)
                                           :& field lucidJsonFormat
                                           :& field (listJsonFormat x)
                                           :& field textJsonFormat
                                           :& RNil

type MainPage = FRecentPosts Stage1Post : Enriched (FTitle : FImage : BasicMD)

mainPageJsonFormat :: JsonFormatRecord e MainPage
mainPageJsonFormat = field (listJsonFormat $ recordJsonFormat stage1PostJsonFormat)
                  :& enrichedXJsonFormat (field textJsonFormat
                                       :& optionalField textJsonFormat
                                       :& basicMDJsonFormat)

instance Ix.Indexable '[Tag, Posted, YearMonth] (Record Stage1Post) where
  indices = Ix.ixList (Ix.ixFun (fmap Tag . viewTags))
                    (Ix.ixFun (pure. Posted. viewPosted))
                      (Ix.ixFun (pure . YearMonth . toYearMonthPair . viewPosted))

type Stage1PostSet = Ix.IxSet '[Tag, Posted, YearMonth] (Record Stage1Post)

data PostIndex a where
  AllPosts    :: PostIndex Stage1PostSet
  AllTags     :: PostIndex [Tag]
  ByTag       :: Tag -> PostIndex Stage1PostSet
  ByYearMonth :: YearMonth -> PostIndex Stage1PostSet
  DescPosted  :: PostIndex Stage1PostSet -> PostIndex [Record Stage1Post]
  DescPostedZ :: PostIndex Stage1PostSet -> PostIndex (Zipper [] (Record Stage1Post))
  RecentPosts :: Int -> PostIndex [Record Stage1Post]
  Paginate    :: Int -> PostIndex [Record Stage1Post] -> PostIndex (Zipper [] [Record Stage1Post])
  PagesRoot   :: PostIndex Stage1PostSet -> PostIndex Text
  PagesLinks  :: Int -> PostIndex Stage1PostSet -> PostIndex (Zipper [] (Record Link))

postIndex :: (MonadAction m, MonadThrow m ) => (Path Rel File -> m (Record Stage1Post)) -> PostIndex a -> m a
postIndex rd AllPosts        = batchLoadIndex rd sourceFolder ["posts/*.md"]
postIndex rd (ByTag t)       = ((Ix.@+ [t]) <$> postIndex rd AllPosts)
postIndex rd (ByYearMonth t) = ((Ix.@+ [t]) <$> postIndex rd AllPosts)
postIndex rd (DescPosted x)  = Ix.toDescList (Proxy @Posted) <$> postIndex rd x 
postIndex rd (DescPostedZ x) = Ix.toZipperDesc (Proxy @Posted) =<< postIndex rd x 
postIndex rd (RecentPosts x) = take x <$> postIndex rd (DescPosted AllPosts)
postIndex rd (Paginate x f)  = postIndex rd f >>= paginate' x
postIndex rd (PagesRoot f)   = return $ case f of
      AllPosts      -> "/posts/"
      ByTag (Tag t) -> "/posts/tags/" <> t <> "/"
      ByYearMonth (YearMonth (y, m)) -> "/posts/months/" <> defaultMonthUrlFormat (fromYearMonthPair (y, m)) <> "/"
postIndex rd (PagesLinks x f) = do
  xs <- postIndex rd (Paginate x (DescPosted f))
  u <- postIndex rd (PagesRoot f)
  return $ extend (\x -> T.pack (show (pos x + 1)) :*: (u <> "pages/" <> T.pack (show (pos x + 1))) :*: RNil) xs
  

type TMain      = "templates/index.html" :-> Record MainPage
type TDoc       = "templates/docs.html"  :-> Record FinalDoc
type TPost      = "templates/post.html"  :-> Record FinalPost
type TPostIndex = "templates/index.html" :-> Record (IndexPage Stage1Post)

rules :: ShakePlus LogFunc ()
rules = do

  readMD <- newCache $ \x -> do
    logInfo $ "Loading " <> displayShow (toFilePath . toFile $ x)
    loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions x

  readRawSingle <- newCache $ readMD >=> parseValue' (recordJsonFormat rawSingleJsonFormat)
  readRawPost   <- newCache $ readMD >=> parseValue' (recordJsonFormat rawPostJsonFormat)
  readRawDoc    <- newCache $ readMD >=> parseValue' (recordJsonFormat rawDocJsonFormat)

  readStage1Post <- newCache $ readRawPost >=> stage1Post
  readStage1Doc  <- newCache $ readRawDoc  >=> stage1Doc

  let o' = (`within` outputFolder)
      s' = (`within` sourceFolder)

      postIx :: PostIndex a -> RAction LogFunc a
      postIx  = postIndex readStage1Post

      blogNav = myBlogNav <$> postIx AllPosts

      indexHtml = $(mkRelFile "index.html") :: Path Rel File

  o' "index.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    v   <- readRawSingle (fromWithin src)
    xs  <- postIx (RecentPosts numRecentPosts)
    let (v' :: TMain) = Val $ xs :*: enrichPage v
    buildPageAction' sourceFolder v' (recordJsonFormat mainPageJsonFormat) out

  o' "posts/*.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    xs  <- postIx (DescPostedZ AllPosts) >>= seekOnThrow viewSrcPath (toFile src)
    nav <- blogNav
    let (v :: TPost) = Val $ nav :*: enrichPage (extract xs)
    buildPageAction' sourceFolder v (recordJsonFormat finalPostJsonFormat) out

  toc' <- mapM (mapM withHtmlExtension) $ fmap o' tableOfContents
  void . sequence . flip extend toc' $ \xs -> (toFilePath <$> extract xs) %^> \out -> do
    let getDoc = readStage1Doc . fromWithin <=< blinkAndMapM sourceFolder withMdExtension
    ys <- mapM getDoc toc'
    zs <- mapM getDoc (fmap extract . unwrap $ xs)
    v  <- getDoc out
    let (v' :: TDoc) = Val $ myDocNav ys :*: zs :*: enrichPage v
    buildPageAction' sourceFolder v' (recordJsonFormat finalDocJsonFormat) out

  o' "posts/index.html" %^>
    copyFileChanged (o' ($(mkRelFile "posts/pages/1/index.html") :: Path Rel File))

  o' "posts/pages/*/index.html" %^> \out -> do
    let n = read . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs  <- postIx $ Paginate postsPerPage (DescPosted AllPosts)
    nav <- blogNav
    ys <- postIx $ PagesLinks postsPerPage AllPosts
    let (v :: TPostIndex) = Val $ enrichPage (unzipper ys :*: nav :*: extract (seek (n -1) xs) :*: "Posts" :*: RNil)
    buildPageAction' sourceFolder v (recordJsonFormat $ indexPageJsonFormat (recordJsonFormat stage1PostJsonFormat)) out

  o' "posts/tags/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/tags/" <> t <> "/pages/1/index.html"
    copyFileChanged (o' i) out

  o' "posts/tags/*/pages/*/index.html" %^> \out -> do
    let zs = splitOn "/" . toFilePath . extract $ out
    let t = T.pack $ zs !! 2
    let n = read   $ zs !! 4
    xs  <- postIx $ Paginate postsPerPage (DescPosted (ByTag (Tag t)))
    nav <- blogNav
    ys <- postIx $ PagesLinks postsPerPage (ByTag (Tag t))
    let (v :: TPostIndex) = Val $ enrichPage (unzipper ys :*: nav :*: extract (seek (n -1) xs) :*: "Posts Tagged " <> t :*: RNil)
    buildPageAction' sourceFolder v (recordJsonFormat $ indexPageJsonFormat (recordJsonFormat stage1PostJsonFormat)) out

  o' "posts/months/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/months/" <> t <> "/pages/1/index.html"
    copyFileChanged (o' i) out

  o' "posts/months/*/pages/*/index.html" %^> \out -> do
    let zs = splitOn "/" . toFilePath . extract $ out
    let t = parseISODateTime $ T.pack $ zs !! 2
    let t' = YearMonth $ toYearMonthPair t
    let n = read   $ zs !! 4
    xs  <- postIx $ Paginate postsPerPage (DescPosted $ ByYearMonth t')
    nav <- blogNav
    ys <- postIx $ PagesLinks postsPerPage (ByYearMonth t')
    let (v :: TPostIndex) = Val $ enrichPage (unzipper ys :*: nav :*: extract (seek (n -1) xs) :*: "Posts from " <> defaultPrettyMonthFormat t :*: RNil)
    buildPageAction' sourceFolder v (recordJsonFormat $ indexPageJsonFormat (recordJsonFormat stage1PostJsonFormat)) out

  o' ["css//*", "js//*", "webfonts//*", "images//*"] |%^> \out ->
    copyFileChanged (blinkLocalDir sourceFolder out) out

  o' "sitemap.xml" %^> \out -> do
    xs <- postIx (DescPosted AllPosts)
    buildSitemap baseUrl xs out

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
