{-# LANGUAGE TemplateHaskell #-}

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

myIndex :: (MonadThrow m, RElem FPosted xs) => [Record xs] -> m (Zipper [] (Record (RawIndexPage xs)))
myIndex = genIndexPageData "Posts" (("/posts/pages/" <>) . T.pack . show) postsPerPage

myTagIndex :: (MonadThrow m, RElem FPosted xs) => Tag -> [Record xs] -> m (Zipper [] (Record (RawIndexPage xs)))
myTagIndex (Tag t) = genIndexPageData ("Posts tagged " <> t) ((("/posts/tags/" <> t <> "/pages/") <>) . T.pack .show) postsPerPage

myMonthIndex :: (MonadThrow m, RElem FPosted xs) => YearMonth -> [Record xs] -> m (Zipper [] (Record (RawIndexPage xs)))
myMonthIndex (YearMonth (y, m)) =
  let t' = fromYearMonthPair (y, m)
  in genIndexPageData (("Posts from " <>) . defaultPrettyMonthFormat $ t')
                      ((("/posts/months/"  <> defaultMonthUrlFormat t' <> "/pages/") <>) . T.pack . show)
                      postsPerPage

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

--- Stage 0 Oracles

deriving instance Binary (Within Rel [FilePattern])

deriving instance NFData (Within Rel [FilePattern])

newtype PostUrls = PostUrls (Within Rel [FilePattern])
  deriving (Eq, Ord, Binary, Hashable, Typeable, NFData)

type instance RuleResult PostUrls = [Text]

getAllPostUrls :: MonadAction m => PostUrls -> m [Text]
getAllPostUrls (PostUrls fp) = do
  xs <- getDirectoryFilesWithin fp 
  return $ toGroundedUrl <$> extract xs

--- Stage 1 Types

type URLised x = FUrl : x

urlisedXJsonFormat :: JsonFormatRecord e x -> JsonFormatRecord e (URLised x)
urlisedXJsonFormat x = field defaultJsonFormat :& x

addUrl :: (MonadThrow m, RElem FSrcPath xs) => Record xs -> m (Record (FUrl : xs))
addUrl = addDerivedUrl (fmap toGroundedUrl . withHtmlExtension <=< stripProperPrefix sourceFolder)

type Stage1Post = FTagLinks : URLised RawPost

stage1PostJsonFormat :: JsonFormatRecord e Stage1Post
stage1PostJsonFormat = field (listJsonFormat $ recordJsonFormat linkJsonFormat)
                    :& urlisedXJsonFormat rawPostJsonFormat

addTagLinks :: RElem FTags xs => Record xs -> Record (FTagLinks : xs)
addTagLinks xs = (fmap (\x -> x :*: ("/posts/tags/" <> x) :*: RNil) . viewTags $ xs ) :*: xs

type Stage1Doc = URLised RawDoc

stage1DocJsonFormat :: JsonFormatRecord e Stage1Doc
stage1DocJsonFormat = urlisedXJsonFormat rawDocJsonFormat

newtype PostByUrl = PostByUrl Text
  deriving (Eq, Ord, Binary, Hashable, Typeable, NFData)

type instance RuleResult PostByUrl = Record Stage1Post

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
                  :& field (listJsonFormat $ recordJsonFormat $ urlisedXJsonFormat rawDocJsonFormat)
                  :& enrichedXJsonFormat (urlisedXJsonFormat rawDocJsonFormat)

type FinalPost = FToc : Enriched Stage1Post

finalPostJsonFormat :: JsonFormatRecord e FinalPost
finalPostJsonFormat = field lucidJsonFormat
                   :& enrichedXJsonFormat stage1PostJsonFormat

rawIndexPageJsonFormat :: JsonFormat e (Record x) -> JsonFormatRecord e (RawIndexPage x)
rawIndexPageJsonFormat x = field textJsonFormat
                        :& field textJsonFormat
                        :& field (listJsonFormat $ x)
                        :& RNil


rules :: HasLogFunc r => ShakePlus r ()
rules = do

  readMD <- newCache $ \x -> do
    logInfo $ "Loading " <> displayShow (toFilePath . toFile $ x)
    loadMarkdownAsJSON defaultMarkdownReaderOptions defaultHtml5WriterOptions x

  readRawPost <- newCache $ readMD >=> parseValue' (recordJsonFormat rawPostJsonFormat)

  readRawDoc  <- newCache $ readMD >=> parseValue' (recordJsonFormat rawDocJsonFormat)

  readStage1Post <- newCache $ readRawPost >=> addUrl >=> return . addTagLinks

  readStage1Doc <- newCache $ readRawDoc >=> addUrl

  genPostsIx   <- newCache $ postIndex readStage1Post

  blogNav   <- newCache $ postsIx >=> return . myBlogNav

  postsZ    <- newCache $ postsIx >=> postZipper

  blogIndex <- newCache $ postsIx >=> myIndex . Ix.toList

  blogTagIndex <- newCache $ postsIx >=> flip Ix.toHashMapByM myTagIndex

  blogMonthIndex <- newCache $ postsIx >=> flip Ix.toHashMapByM myMonthIndex

  let o' = (`within` outputFolder)
      s' = (`within` sourceFolder)

      myPosts = s' ["posts/*.md"]

  o' "index.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    v   <- readMD src
    return ()
  --  let v' = pygments :*: mySocial :*: siteTitle :*: defaultCdnImports :*: take numRecentPosts (unzipper rs) :*: v
--    buildPageAction' (s' tmpl) v finalDocJsonFormat out
--    buildPageAction' (s' $(mkRelFile "templates/index.html")) v out

  o' "posts/*.html" %^> \out -> do
    src <- blinkAndMapM sourceFolder withMdExtension out
    xs <- postsZ myPosts >>= seekOnThrow viewSrcPath (toFile src)
    let v = myBlogNav xs :*: enrichPage (extract xs)
    buildPageAction' (s' $(mkRelFile "templates/post.html")) v (recordJsonFormat finalPostJsonFormat) out

  toc' <- mapM (mapM withHtmlExtension) $ fmap o' tableOfContents
  void . sequence . flip extend toc' $ \xs -> (toFilePath <$> extract xs) %^> \out -> do
    let getDoc = readDoc <=< blinkAndMapM sourceFolder withMdExtension
    ys <- mapM getDoc toc'
    zs <- mapM getDoc (fmap extract . unwrap $ xs)
    v  <- getDoc out
    let v' = myDocNav ys :*: zs :*: enrichPage v
    buildPageAction' (s' $(mkRelFile "templates/docs.html")) v' (recordJsonFormat finalDocJsonFormat) out

  o' "posts/index.html" %^>
    copyFileChanged (o' ($(mkRelFile "posts/pages/1/index.html") :: Path Rel File))

  o' "posts/pages/*/index.html" %^> \out -> do
--    let n = read . (!! 2) . splitOn "/" . toFilePath . extract $ out
    xs <- blogIndex myPosts
    return ()
--    buildPageAction' (s' $(mkRelFile "templates/post-index.html")) (seek (n -1) xs) (recordJsonFormat finalPostIndexPageJsonFormat) out

  o' "posts/tags/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/tags/" <> t <> "/pages/1/index.html"
    copyFileChanged (o' i) out

  o' "posts/tags/*/pages/*/index.html" %^> \out -> do
     let zs = splitOn "/" . toFilePath . extract $ out
     let t = Tag $ T.pack $ zs !! 2
  --   let n = read   $ zs !! 4
     return ()
  --   xs <- blogTagIndex myPosts
  --   case HM.lookup t xs of
--       Nothing -> logError $ "Attempting to lookup non-existant tag " <> displayShow t
  --     Just x  -> myBuildPostListPage (seek (n - 1) x) out

  o' "posts/months/*/index.html" %^> \out -> do
    let t = (!! 2) . splitOn "/" . toFilePath . extract $ out
    i <- parseRelFile $ "posts/months/" <> t <> "/pages/1/index.html"
    copyFileChanged (o' i) out

  o' "posts/months/*/pages/*/index.html" %^> \out -> do
     let zs = splitOn "/" . toFilePath . extract $ out
     let t = YearMonth $ toYearMonthPair $ parseISODateTime $ T.pack $ zs !! 2
   --  let n = read   $ zs !! 4
     return ()
  --   xs <- blogMonthIndex myPosts
   --  case HM.lookup t xs of
   --    Nothing -> logError $ "Attempting to lookup non-existant month " <> displayShow t
   --    Just x  -> myBuildPostListPage (seek (n - 1) x) out

  o' ["css//*", "js//*", "webfonts//*", "images//*"] |%^> \out ->
    copyFileChanged (blinkLocalDir sourceFolder out) out

  o' "sitemap.xml" %^> \out -> do
    xs <- postsZ myPosts
    buildSitemap baseUrl (unzipper xs) out

  let simplePipeline f = getDirectoryFiles sourceFolder >=> mapM f >=> needIn outputFolder
      verbatimPipeline = simplePipeline return

  phony "statics" $ verbatimPipeline ["css//*", "js//*", "webfonts//*", "images//*"]

  phony "index" $ needIn outputFolder [$(mkRelFile "index.html") :: Path Rel File]

  phony "post-index" $ do
     ps <- blogIndex myPosts
     fs <- defaultPagePaths [1..size ps]
     let postFolder = (outputFolder </> $(mkRelDir "posts") :: Path Rel Dir)
     needIn postFolder ($(mkRelFile "index.html") : fs)

  phony "by-tag-index" $ do
     ps <- blogTagIndex myPosts
     void $ flip HM.traverseWithKey ps $ \(Tag t) z -> do
       u  <- parseRelDir $ T.unpack t
       fs <- defaultPagePaths [1..size z]
       let tagFolder = outputFolder </> $(mkRelDir "posts/tags") </> u
       needIn tagFolder ($(mkRelFile "index.html") : fs)

  phony "by-month-index" $ do
     ps <- blogMonthIndex myPosts
     void $ flip HM.traverseWithKey ps $ \(YearMonth t) z -> do
       u  <- parseRelDir $ T.unpack $ defaultMonthUrlFormat $ fromYearMonthPair $ t
       fs <- defaultPagePaths [1..size z]
       let monthFolder = outputFolder </> $(mkRelDir "posts/months") </> u
       needIn monthFolder ($(mkRelFile "index.html") : fs)

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
   --shake shakeOptions $ want ["index", "docs", "posts", "post-index", "by-tag-index", "by-month-index", "sitemap"] >> runShakePlus lf rules
   shake shakeOptions $ want ["docs"] >> runShakePlus lf rules
   defaultMain $ tests xs
   dlf
