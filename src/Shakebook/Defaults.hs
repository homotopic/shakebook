{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Defaults where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Store.Class
import           Control.Comonad.Zipper.Extra
import           Control.Monad.Extra
import           Data.Aeson                   as A
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake.Plus
import qualified Development.Shake.FilePath   as S
import           RIO
import qualified RIO.ByteString.Lazy          as LBS
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Map                      as M
import           RIO.Partial
import qualified RIO.Text                     as T
import           RIO.Time
import           Path                         as P
import           Shakebook.Aeson
import           Shakebook.Conventions
import           Shakebook.Data
import           Shakebook.Mustache
import           Text.DocTemplates
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.PDF
import           Text.Pandoc.Readers
import           Text.Pandoc.Templates
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers
import           Within

defaultMonthUrlFormat :: UTCTime -> String
defaultMonthUrlFormat = formatTime defaultTimeLocale "%Y-%m"

defaultPrettyMonthFormat :: UTCTime -> String
defaultPrettyMonthFormat = formatTime defaultTimeLocale "%B, %Y"

defaultPrettyTimeFormat :: UTCTime -> String
defaultPrettyTimeFormat = formatTime defaultTimeLocale "%A, %B %d, %Y"

defaultIndexFileFragment :: Path Rel File
defaultIndexFileFragment = $(mkRelFile "index.html")

defaultMonthDirFragment :: MonadThrow m => UTCTime -> m (Path Rel Dir)
defaultMonthDirFragment t = do
  k <- parseRelDir $ defaultMonthUrlFormat t
  return $ $(mkRelDir "posts/months") </> k

defaultMonthUrlFragment :: UTCTime -> Text
defaultMonthUrlFragment t = T.pack $ "/posts/months/" <> defaultMonthUrlFormat t

defaultEnrichPost :: Value -> Value
defaultEnrichPost = enrichTeaser "<!--more-->"
                  . enrichTagLinks ("/posts/tags/" <>)
                  . enrichPrettyDate defaultPrettyTimeFormat
--                  . enrichTypicalUrl

defaultMarkdownReaderOptions :: ReaderOptions
defaultMarkdownReaderOptions = def { readerExtensions = pandocExtensions }

defaultHtml5WriterOptions :: WriterOptions
defaultHtml5WriterOptions = def { writerHTMLMathMethod = MathJax ""}

defaultLatexWriterOptions :: WriterOptions
defaultLatexWriterOptions = def { writerTableOfContents = True
                         , writerVariables = Context $ M.fromList [
                                               ("geometry", SimpleVal "margin=3cm")
                                             , ("fontsize", SimpleVal "10")
                                             , ("linkcolor",SimpleVal "blue")]
                         }

defaultSbSrcDir :: Path Rel Dir
defaultSbSrcDir = $(mkRelDir "site")

defaultSbOutDir :: Path Rel Dir
defaultSbOutDir = $(mkRelDir "public")

defaultPostsPerPage :: Int
defaultPostsPerPage = 5

defaultSbConfig :: Text -- ^ BaseURL
                -> Text -- ^ SiteTitle
                -> SbConfig
defaultSbConfig x y = SbConfig
  defaultSbSrcDir
  defaultSbOutDir
  x
  defaultMarkdownReaderOptions
  defaultHtml5WriterOptions
  defaultPostsPerPage
  (withSiteTitle y)

affixBlogNavbar :: MonadShakebookAction r m
                => [FilePattern]
                -> Text
                -> Text
                -> (UTCTime -> Text)
                -> (UTCTime -> Text)
                -> (Value -> Value) -- ^ Post enrichment.
                -> Value -> m Value
affixBlogNavbar patterns a b c d e x = do
  xs <- loadSortEnrich patterns (Down . viewPostTime) e
  return $ withJSON (genBlogNavbarData a b c d (snd <$> xs)) $ x

affixRecentPosts :: MonadShakebookAction r m
                 => [FilePattern]
                 -> Int
                 -> (Value -> Value) -- ^ Post enrichment
                 -> Value -> m Value
affixRecentPosts patterns n e x = do
  xs <- loadSortEnrich patterns (Down . viewPostTime) e
  return $ withRecentPosts (take n (snd <$> xs)) $ x

affixTagIndex :: MonadShakebookAction r m
              => [FilePattern]
              -> Value -> m Value
affixTagIndex patterns x = do
  xs <- loadSortEnrich patterns (Down . viewPostTime) id
  let ts = sort $ viewAllPostTags (snd <$> xs)
  return $ withTagIndex ((genLinkData <*> ("/posts/tags/" <>)) <$> ts) $ x

defaultDocsPatterns :: (MonadShakebookRules r m)
                    => Cofree [] FilePath -- Rosetree Table of Contents.
                    -> FilePath
                    -> (Value -> Value) -- Extra data modifiers.
                    -> m ()
defaultDocsPatterns toc tmpl withData = view sbConfigL >>= \SbConfig{..} -> do
  tmpl' <- parseRelFile tmpl
  o <- view localOutL
  toc'  <- mapM (parseRelFile >=> pure . (`within` o) >=> mapWithinT withHtmlExtension) toc
  let e = blinkAndMapT sbSrcDir withMarkdownExtension >=> readMarkdownFile' >=> enrichSupposedUrl
  void . sequence . flip extend toc' $ \xs -> (toFilePath . whatLiesWithin $ extract xs) %->  \out -> do
             ys <- mapM e toc'
             zs <- mapM e xs
             v  <- e out
             let v' = withData . withJSON (genTocNavbarData ys) . withSubsections (lower (zs)) $ v
             buildPageActionWithin (tmpl' `within` sbSrcDir) v' out

defaultPostIndexData :: MonadShakebookAction r m
                     => [FilePattern]
                     -> (a -> Value -> Bool) -- ^ A filtering function 
                     -> (a -> Text) -- ^ How to turn the id into a Title.
                     -> (a -> Text -> Text) -- ^ How to turn the id and a page number (as Text) into a URL link.
                     -> a -- ^ The id itself.
                     -> m (Zipper [] Value) -- A pager of index pages.
defaultPostIndexData pat f t l a = view sbConfigL >>= \SbConfig {..} -> do
  xs <- loadSortFilterEnrich pat (Down . viewPostTime) (f a) defaultEnrichPost
  ys <- genIndexPageData (snd <$> xs) (t a) (l a) sbPPP
  return (fmap sbGlobalApply ys)

defaultPagerPattern :: (MonadShakebookRules r m)
                    => FilePattern
                    -> FilePath
                    -> (FilePattern -> Int) -- ^ How to extract a page number from the Filepattern.
                    -> (FilePattern -> a) -- ^ How to extract an id from the FilePattern.
                    -> (a -> RAction r (Zipper [] Value)) -- ^ How to get from a value to a page.
                    -> (Value -> RAction r Value) -- ^ A monadic affix to apply to each pae.
                    -> (Zipper [] Value -> RAction r (Zipper [] Value)) -- ^ An extensin over the whole zipper.
                    -> m ()
defaultPagerPattern fp tmpl f g h a w = view sbConfigL >>= \SbConfig{..} -> do
  tmpl' <- parseRelFile tmpl
  fp %-> \x -> do
    let x' = toFilePath $ whatLiesWithin x
    xs <- (w <=< mapM a <=< h) $ g (x')
    let b = extract (seek (f x') xs)
    buildPageActionWithin (tmpl' `within` sbSrcDir) b x

defaultPostIndexPatterns :: MonadShakebookRules r m
                         => [FilePattern]
                         -> FilePath
                         -> (Value -> RAction r Value) -- ^ A monadic affix to apply to each pae.
                         -> (Zipper [] Value -> RAction r (Zipper [] Value)) -- ^ Pager extension.
                         -> m ()
defaultPostIndexPatterns pat tmpl aff extData = do
   defaultPagerPattern "posts/index.html" tmpl
                       (const 0)
                       (const ())
                       (defaultPostIndexData pat (const $ (const True))
                                                 (const "Posts")
                                                 (const ("/posts/pages/" <>)))
                       aff
                       extData
   defaultPagerPattern ("posts/pages/*/index.html") tmpl
                       ((+ (-1)) . read . (!! 2) . splitOn "/")
                       (const ())
                       (defaultPostIndexData pat (const $ (const True))
                                                 (const "Posts")
                                                 (const ("/posts/pages/" <>)))
                       aff
                       extData

defaultTagIndexPatterns :: MonadShakebookRules r m
                        => [FilePattern]
                        -> FilePath
                        -> (Value -> RAction r Value) -- ^ A monadic affix to apply to each pae.
                        -> (Zipper [] Value -> RAction r (Zipper [] Value)) -- ^ Pager extension.
                        -> m ()
defaultTagIndexPatterns pat tmpl aff extData = do
 defaultPagerPattern ("posts/tags/*/index.html") tmpl
                     (const 0)
                     (T.pack . (!! 2) . splitOn "/")
                     (defaultPostIndexData pat (\x y -> elem x (viewTags y) )
                                               ("Posts tagged " <>)
                                               (\x y -> ("/posts/tags/" <> x <> "/pages/" <> y)))
                     aff
                     extData
 defaultPagerPattern ("posts/tags/*/pages/*/index.html") tmpl
                     ((+ (-1)) . read . (!! 4) . splitOn "/")
                     (T.pack . (!! 2) . splitOn "/")
                     (defaultPostIndexData pat (\x y -> elem x (viewTags y))
                                               ("Posts tagged " <>)
                                               (\x y -> ("/posts/tags/" <> x <> "/pages/" <> y)))
                     aff
                     extData

defaultMonthIndexPatterns :: MonadShakebookRules r m
                          => [FilePattern]
                          -> FilePath
                          -> (Value -> RAction r Value) -- ^ A monadic affix to apply to each pae.
                          -> (Zipper [] Value -> RAction r (Zipper [] Value)) -- ^ Pager extension.
                          -> m ()
defaultMonthIndexPatterns pat tmpl aff extData = do
 defaultPagerPattern "posts/months/*/index.html" tmpl
                     (const 0)
                     (parseISODateTime . T.pack . (!! 2) . splitOn "/")
                     (defaultPostIndexData pat
                        (\x y -> sameMonth x (viewPostTime y))
                        (("Posts from " <>) . T.pack . defaultPrettyMonthFormat)
                        (\x y -> ("/posts/months/" <> T.pack (defaultMonthUrlFormat x) <> "/pages" <> y)))
                     aff
                     extData
 defaultPagerPattern "posts/months/*/pages/*/index.html" tmpl
                      ((+ (-1)) . read . (!! 4) . splitOn "/")
                      (parseISODateTime . T.pack . (!! 2) . splitOn "/")
                       (defaultPostIndexData pat
                           (\x y -> sameMonth x (viewPostTime y))
                           (("Posts from " <>) . T.pack . defaultPrettyMonthFormat)
                           (\x y -> ("/posts/months/" <> T.pack (defaultMonthUrlFormat x) <> "/pages" <> y)))
                       aff
                       extData

{-|
   Default Posts Pager. 
-}
defaultPostsPatterns :: MonadShakebookRules r m
                     => FilePattern
                     -> FilePath
                     -> (Value -> RAction r Value) -- ^ A post loader function.
                     -> (Zipper [] Value -> RAction r (Zipper [] Value)) -- ^ A transformation on the entire post zipper.
                     -> m ()
defaultPostsPatterns pat tmpl e extData = view sbConfigL >>= \SbConfig {..} ->
   pat %-> \out -> do
    logInfo $ display $ "Caught pattern: " <> display (WithinDisplay out)
    tmpl' <- parseRelFile tmpl
    logInfo $ display $ "Using template " <> display (PathDisplay tmpl')
    let pat' = pat S.-<.> ".md"
    xs    <- loadSortEnrich [pat'] (Down . viewPostTime) defaultEnrichPost
    xs'   <- mapM (\(s,x) -> e x >>= \e' -> return (s, e')) xs
    i     <- blinkAndMapT sbSrcDir withMarkdownExtension out
    logInfo $ display $ WithinDisplay i
    logInfo $ display $ WithinDisplay . fst <$> xs'
    let k = fromJust $ elemIndex i (fst <$> xs')
    let z = fromJust $ seek k <$> zipper (snd <$> xs')
    z' <- extData z
    buildPageActionWithin (tmpl' `within` sbSrcDir) (extract z') out

makePDFLaTeX :: Pandoc -> PandocIO (Either LBS.ByteString LBS.ByteString)
makePDFLaTeX p = do
  t <- compileDefaultTemplate "latex"
  makePDF "pdflatex" [] writeLaTeX defaultLatexWriterOptions { writerTemplate = Just t } p

handleImages :: Text -> (Text -> Text) -> Inline -> Inline
handleImages prefix f (Image attr ins (src,txt)) =
  if T.takeEnd 4 src == ".mp4" then Str (f src)
  else Image attr ins (prefix <> "/" <> src, txt)
handleImages _ _ x = x

handleHeaders :: Int -> Block -> Block
handleHeaders i (Header a as xs) = Header (max 1 (a + i)) as xs
handleHeaders _ x                = x

pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

-- | Build a PDF from a Cofree table of contents.
buildPDF :: (MonadShakebookAction r m, MonadFail m) => Cofree [] String -> Path Rel File -> FilePath -> m ()
buildPDF toc meta out = view sbConfigL >>= \SbConfig {..} -> do
  y <- mapM (readFileIn' sbSrcDir <=< parseRelFile) toc
  m <- readFileIn' sbSrcDir meta
  Right f <- liftIO . runIOorExplode $ do
    k <- mapM (readMarkdown sbMdRead ) y
    a <- readMarkdown sbMdRead $ m
    let z = walk (handleImages (T.pack $ toFilePath sbOutDir) (\x -> "[Video available at " <> sbBaseUrl <> x <> "]")) $ foldr (<>) a $ pushHeaders (-1) k
    makePDFLaTeX z
  LBS.writeFile out f

{-|
  Default Single Page Pattern, see tests for usage. It's possible this could just be
  called singlePagePattern, as there's no hardcoded strings here, but it would need to
  run entirely within the monad to translate filepaths.
-}
defaultSinglePagePattern :: (MonadRules m, MonadReader r m, HasSbConfig r, HasLocalOut r)
                         => FilePath -- ^ The output filename e.g "index.html".
                         -> FilePath -- ^ A tmpl file.
                         -> (Value -> RAction r Value) -- ^ Last minute enrichment.
                         -> m ()
defaultSinglePagePattern out tmpl withDataM = view sbConfigL >>= \SbConfig {..} -> do
  out %-> \x -> do
    tmpl' <- parseRelFile tmpl
    x'    <- blinkAndMapT sbSrcDir withMarkdownExtension $ x
    v     <- withDataM =<< readMarkdownFile' x'
    buildPageActionWithin (tmpl' `within` sbSrcDir) v x

{-|
  Default statics patterns. Takes a list of filepatterns and adds a rule that copies everything
  verbatim
-}
defaultStaticsPatterns :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultStaticsPatterns xs =  view sbConfigL >>= \SbConfig {..} -> do
  foldr (>>) (return ()) $ flip map xs $ flip (%->) $ \y -> do
       let y' = blinkWithin sbSrcDir y
       copyFileChanged (fromWithin y') (fromWithin y)

-- | Default "shake clean" phony, cleans your output directory.
defaultCleanPhony :: MonadShakebookRules r m => m ()
defaultCleanPhony = view sbConfigL >>= \SbConfig {..} -> 
  phony "clean" $ do
      logInfo $ "Cleaning files in " <> display (PathDisplay sbOutDir)
      removeFilesAfter sbOutDir ["//*"]

defaultSinglePagePhony :: MonadShakebookRules r m => String -> FilePath -> m ()
defaultSinglePagePhony x y = phony x $ parseRelFile y >>= needLocalOut . pure

{-|
  Default "shake statics" phony rule. automatically runs need on "\<out\>\/thing\/\*" for every
  thing found in "images\/", "css\/", "js\/" and "webfonts\/"
-}
defaultStaticsPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultStaticsPhony pattern = view sbConfigL >>= \SbConfig{..} -> 
  phony "statics" $ 
    getDirectoryFiles sbSrcDir pattern >>= needIn sbOutDir

{-|
  Default "shake posts" phony rule. takes a [FilePattern] pointing to the posts and
  and calls need on "\<out\>\/posts\/\<filename\>.html" for each markdown post found.
-}  
defaultPostsPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultPostsPhony pattern = view sbConfigL >>= \SbConfig{..} -> 
  phony "posts" $ 
    getDirectoryFilesWithin sbSrcDir pattern >>= mapM (blinkAndMapT sbOutDir withHtmlExtension) >>= needWithin


{-|
  Default "shake posts-index" phony rule. Takes a [FilePattern] of posts to
  discover and calls need on "\<out\>\/posts\/index.html" and
  "\<out\>\/posts\/pages\/\<n\>\/index.html" for each page required.
-}
defaultPostIndexPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultPostIndexPhony pattern = view sbConfigL >>= \SbConfig{..} ->
    phony "posts-index" $ do
      fp  <- getDirectoryFilesWithin sbSrcDir pattern >>= mapM readMarkdownFile'
      needIn sbOutDir [dirPosts </> fileIndexHTML]
      paginate' sbPPP fp >>= defaultPagePaths dirPosts >>= needIn sbOutDir

{-|
  Default "shake tag-index" phony rule. Takes a [FilePattern] of posts to
  discover and calls need on "\<out\>\/posts\/tags\/\<tag\>\/index.html" and
  "\<out\>\/posts\/tags\/\<tag\>\/pages\/\<n\>\/index.html" for each tag discovered and for
  each page required per tag filter.
-}
defaultTagIndexPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultTagIndexPhony pattern = view sbConfigL >>= \SbConfig{..} ->
   phony "tag-index" $ do
      fp  <- getDirectoryFilesWithin sbSrcDir pattern >>= mapM readMarkdownFile'
      forM_ (viewAllPostTags fp) $ \t -> do
        u <- parseRelDir $ T.unpack t
        needIn sbOutDir [dirPosts </> dirTags </> u </> fileIndexHTML]
        paginate' sbPPP (tagFilterPosts t fp)
         >>= defaultPagePaths (dirPosts </> dirTags </> u)
           >>= needIn sbOutDir

defaultPagePaths :: MonadThrow m => Path Rel Dir -> Zipper [] [a] -> m [Path Rel File]
defaultPagePaths a xs = forM [1..size xs] $ parseRelDir . show >=> \p -> return $ a </> dirPages </> p </> fileIndexHTML

fileIndexHTML :: Path Rel File
fileIndexHTML = $(mkRelFile "index.html")

dirPosts :: Path Rel Dir
dirPosts = $(mkRelDir "posts")

dirMonths :: Path Rel Dir
dirMonths = $(mkRelDir "months")

dirPages :: Path Rel Dir
dirPages = $(mkRelDir "pages")

dirTags :: Path Rel Dir
dirTags = $(mkRelDir "tags")

{-|
  Default "shake month-index" phony rule. Takes a [FilePattern] of posts to
  discover and calls need on "\<out\>\/posts\/months\/\<yyyy-md\>\/index.html" and
  "\<out\>\/posts\/months\/\<yyyy-md\>\/pages\/\<n\>\/index.html" for each month
  discovered that contains a post and for each page required per month filter.
-}
defaultMonthIndexPhony :: (MonadRules m, MonadReader r m, HasSbConfig r, HasLocalOut r) => [FilePattern] -> m ()
defaultMonthIndexPhony pattern = phony "month-index" $ do
  SbConfig{..} <- view sbConfigL
  fp  <- getDirectoryFilesWithin sbSrcDir pattern >>= mapM readMarkdownFile'
  forM_ (viewAllPostTimes fp) $ \t -> do
    u <- parseRelDir $ defaultMonthUrlFormat t
    needLocalOut [dirPosts </> dirMonths </> u </> fileIndexHTML]
    paginate' sbPPP (monthFilterPosts t fp)
      >>= defaultPagePaths (dirPosts </> dirMonths </> u)
        >>= needLocalOut

-- | Default "shake docs" phony rule, takes a Cofree [] String as a table of contents.
defaultDocsPhony :: MonadShakebookRules r m
                 => Cofree [] String 
                 -> m ()
defaultDocsPhony toc = view sbConfigL >>= \SbConfig{..} ->
    phony "docs" $ do
      let xs = foldr ((<>) . pure) [] $ toc
      pure xs >>= mapM (parseRelFile >=> withHtmlExtension) >>= needIn sbOutDir
