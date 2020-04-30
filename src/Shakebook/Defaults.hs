{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
module Shakebook.Defaults where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Store.Class
import           Control.Comonad.Store.Zipper
import           Control.Monad.Extra
import           Data.Aeson                   as A
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake            as S
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           RIO
import qualified RIO.ByteString.Lazy          as LBS
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Map                      as M
import           RIO.Partial
import qualified RIO.Text                     as T
import           RIO.Time
import           Shakebook.Aeson
import           Shakebook.Conventions
import           Shakebook.Data
import           Shakebook.Rules
import           Shakebook.Zipper
import           Text.DocTemplates
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.PDF
import           Text.Pandoc.Readers
import           Text.Pandoc.Templates
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers

defaultMonthURLFormat :: UTCTime -> String
defaultMonthURLFormat = formatTime defaultTimeLocale "%Y-%m"

defaultPrettyMonthFormat :: UTCTime -> String
defaultPrettyMonthFormat = formatTime defaultTimeLocale "%B, %Y"

defaultPrettyTimeFormat :: UTCTime -> String
defaultPrettyTimeFormat = formatTime defaultTimeLocale "%A, %B %d, %Y"

defaultMonthIndexUrlFormat :: UTCTime -> String
defaultMonthIndexUrlFormat t = "/posts/months" </> defaultMonthURLFormat t

defaultEnrichPost :: Value -> Value
defaultEnrichPost = enrichTeaser "<!--more-->"
                  . enrichTagLinks ("/posts/tags/" <>)
                  . enrichPrettyDate defaultPrettyTimeFormat

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

defaultSbConfig :: Text -- ^ BaseURL
                -> SbConfig
defaultSbConfig x = SbConfig "site" "public" x defaultMarkdownReaderOptions defaultHtml5WriterOptions 5

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



defaultDocsPatterns :: MonadShakebookRules r m
                    => Cofree [] FilePath -- Rosetree Table of Contents.
                    -> FilePath
                    -> (Value -> Value) -- Extra data modifiers.
                    -> m ()
defaultDocsPatterns toc tmpl withData = ask >>= \r -> view sbConfigL >>= \SbConfig {..} -> do
  let e = typicalUrlEnricher
  m <- typicalFullOutHTMLToMdSrcPath
  liftRules $ cofreeRuleGen toc ((sbOutDir </>) . (-<.> ".html")) (
         \xs -> \out -> runShakebookA r $ do
             ys <- mapM readMarkdownFile' toc
             zs <- mapM readMarkdownFile' xs
             void $ genBuildPageAction (sbSrcDir </> tmpl)
                      (readMarkdownFile' . m)
                      (withData
                     . withJSON (genTocNavbarData (e <$> ys))
                     . withSubsections (lower (e <$> zs)))
                      out)

defaultPostIndexData :: MonadShakebookAction r m
                     => [FilePattern]
                     -> (a -> Value -> Bool) -- ^ A filtering function 
                     -> (a -> Text) -- ^ How to turn the id into a Title.
                     -> (a -> Text -> Text) -- ^ How to turn the id and a page number (as Text) into a URL link.
                     -> a -- ^ The id itself.
                     -> m (Zipper [] Value) -- A pager of index pages.
defaultPostIndexData pat f t l a = view sbConfigL >>= \SbConfig {..} -> do
  xs <- loadSortFilterEnrich pat (Down . viewPostTime) (f a) defaultEnrichPost
  let ys = genIndexPageData (snd <$> xs) (t a) (l a) sbPPP
  return $ fromJust $ ys

defaultPagerPattern :: MonadShakebookRules r m
                    => FilePattern
                    -> FilePath
                    -> (FilePattern -> Int) -- ^ How to extract a page number from the Filepattern.
                    -> (FilePattern -> a) -- ^ How to extract an id from the FilePattern
                    -> (a -> ShakebookA r (Zipper [] Value))
                    -> (Zipper [] Value -> ShakebookA r (Zipper [] Value))
                    -> m ()
defaultPagerPattern fp tmpl f g h w = ask >>= \r -> view sbConfigL >>= \SbConfig{..} -> liftRules $
  comonadStoreRuleGen (sbOutDir </> fp) (f . drop 1 . fromJust . stripPrefix sbOutDir) (g . drop 1 . fromJust . stripPrefix sbOutDir) (runShakebookA r . (w <=< h))
  (\a -> void <$> runShakebookA r . genBuildPageAction (sbSrcDir </> tmpl) (const $ return a) id)

defaultPostIndexPatterns :: MonadShakebookRules r m
                         => [FilePattern]
                         -> FilePath
                         -> (Zipper [] Value -> ShakebookA r (Zipper [] Value)) -- ^ Pager extension.
                         -> m ()
defaultPostIndexPatterns pat tmpl extData = do
   defaultPagerPattern "posts/index.html" tmpl
                       (const 0)
                       (const ())
                       (defaultPostIndexData pat (const $ (const True))
                                                 (const "Posts")
                                                 (const ("/posts/pages/" <>)))
                       extData
   defaultPagerPattern ("posts/pages/*/index.html") tmpl
                       ((+ (-1)) . read . (!! 2) . splitOn "/")
                       (const ())
                       (defaultPostIndexData pat (const $ (const True))
                                                 (const "Posts")
                                                 (const ("/posts/pages/" <>)))
                       extData

defaultTagIndexPatterns :: MonadShakebookRules r m
                        => [FilePattern]
                        -> FilePath
                        -> (Zipper [] Value -> ShakebookA r (Zipper [] Value)) -- ^ Pager extension.
                        -> m ()
defaultTagIndexPatterns pat tmpl extData = do
 defaultPagerPattern ("posts/tags/*/index.html") tmpl
                     (const 0)
                     (T.pack . (!! 2) . splitOn "/")
                     (defaultPostIndexData pat (\x y -> elem x (viewTags y) )
                                               ("Posts tagged " <>)
                                               (\x y -> ("/posts/tags/" <> x <> "/pages/" <> y)))
                     extData
 defaultPagerPattern ("posts/tags/*/pages/*/index.html") tmpl
                     ((+ (-1)) . read . (!! 4) . splitOn "/")
                     (T.pack . (!! 2) . splitOn "/")
                     (defaultPostIndexData pat (\x y -> elem x (viewTags y))
                                               ("Posts tagged " <>)
                                               (\x y -> ("/posts/tags/" <> x <> "/pages/" <> y)))
                     extData

defaultMonthIndexPatterns :: MonadShakebookRules r m
                          => [FilePattern]
                          -> FilePath
                          -> (Zipper [] Value -> ShakebookA r (Zipper [] Value)) -- ^ Pager extension.
                          -> m ()
defaultMonthIndexPatterns pat tmpl extData = do
 defaultPagerPattern "posts/months/*/index.html" tmpl
                     (const 0)
                     (parseISODateTime . T.pack . (!! 2) . splitOn "/")
                     (defaultPostIndexData pat
                        (\x y -> sameMonth x (viewPostTime y))
                        (("Posts from " <>) . T.pack . defaultPrettyMonthFormat)
                        (\x y -> ("/posts/months/" <> T.pack (defaultMonthURLFormat x) <> "/pages" <> y)))
                     extData
 defaultPagerPattern "posts/months/*/pages/*/index.html" tmpl
                      ((+ (-1)) . read . (!! 4) . splitOn "/")
                      (parseISODateTime . T.pack . (!! 2) . splitOn "/")
                       (defaultPostIndexData pat
                           (\x y -> sameMonth x (viewPostTime y))
                           (("Posts from " <>) . T.pack . defaultPrettyMonthFormat)
                           (\x y -> ("/posts/months/" <> T.pack (defaultMonthURLFormat x) <> "/pages" <> y)))
                       extData

{-|
   Default Posts Pager. 
-}
defaultPostsPatterns :: MonadShakebookRules r m
                     => FilePattern
                     -> FilePath
                     -> (Value -> ShakebookA r Value) -- ^ A post loader function.
                     -> (Zipper [] Value -> ShakebookA r (Zipper [] Value)) -- ^ A transformation on the entire post zipper.
                     -> m ()
defaultPostsPatterns pat tmpl e extData = ask >>= \r -> view sbConfigL >>= \SbConfig {..} ->
  liftRules $ sbOutDir </> pat %> \out -> do
      sortedPosts <- runShakebookA r $ do
        xs <- loadSortEnrich [pat] (Down . viewPostTime) defaultEnrichPost
        mapM (\(s,x) -> e x >>= \e' -> return (s, e')) xs
      let i = (-<.> ".md") . drop 1 . fromJust . stripPrefix sbOutDir $ out
      let k = fromJust $ elemIndex i (fst <$> sortedPosts)
      let z = fromJust $ seek k <$> zipper (snd <$> sortedPosts)
      void $ runShakebookA r $ genBuildPageAction (sbSrcDir </> tmpl)
                                (const $ extract <$> extData z)
                                id out

makePDFLaTeX :: Pandoc -> PandocIO (Either LBS.ByteString LBS.ByteString)
makePDFLaTeX p = do
  t <- compileDefaultTemplate "latex"
  makePDF "pdflatex" [] writeLaTeX defaultLatexWriterOptions { writerTemplate = Just t } p

handleImages :: (Text -> Text) -> Inline -> Inline
handleImages f (Image attr ins (src,txt)) =
  if T.takeEnd 4 src == ".mp4" then Str (f src)
  else Image attr ins ("public/" <> src, txt)
handleImages _ x = x

handleHeaders :: Int -> Block -> Block
handleHeaders i (Header a as xs) = Header (max 1 (a + i)) as xs
handleHeaders _ x                = x

pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

-- | Build a PDF from a Cofree table of contents.
buildPDF :: MonadShakebookAction r m => Cofree [] String -> String -> FilePath -> m ()
buildPDF toc meta out = view sbConfigL >>= \SbConfig {..} -> liftAction $ do
  y <- mapM readFile' ((sbSrcDir </>) <$> toc)
  m <- readFile' $  sbSrcDir </> meta
  Right f <- liftIO . runIOorExplode $ do
    k <- mapM (readMarkdown sbMdRead . T.pack) y
    a <- readMarkdown sbMdRead . T.pack $ m
    let z = walk (handleImages (\x -> "[Video available at " <> sbBaseUrl <> x <> "]")) $ foldr (<>) a $ pushHeaders (-1) k
    makePDFLaTeX z
  LBS.writeFile out f

{-|
  Default Single Page Pattern, see tests for usage. It's possible this could just be
  called singlePagePattern, as there's no hardcoded strings here, but it would need to
  run entirely within the monad to translate filepaths.
-}
defaultSinglePagePattern :: MonadShakebookRules r m
                         => FilePath -- ^ The output filename e.g "index.html".
                         -> FilePath -- ^ A tmpl file.
                         -> (Value -> ShakebookA r Value) -- ^ Last minute enrichment.
                         -> m ()
defaultSinglePagePattern out tmpl withDataM = ask >>= \r -> view sbConfigL >>= \SbConfig {..} -> do
  m <- typicalFullOutHTMLToMdSrcPath
  liftRules $ sbOutDir </> out %> void . runShakebookA r . genBuildPageAction
                 tmpl
                 (\fp -> do
                    x <- readMarkdownFile' . m $ fp
                    withDataM $ x)
                 id

{-|
  Default statics patterns. Takes a list of filepatterns and adds a rule that copies everything
  verbatim
-}
defaultStaticsPatterns :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultStaticsPatterns xs = view sbConfigL >>= \SbConfig {..} -> do
  f <- typicalFullOutToSrcPath
  liftRules $ mconcat $ map (\x -> sbOutDir </> x %> \y -> copyFileChanged (f y) y) xs

-- | Default "shake clean" phony, cleans your output directory.
defaultCleanPhony :: MonadShakebookRules r m => m ()
defaultCleanPhony = view sbConfigL >>= \SbConfig {..} -> liftRules $
  phony "clean" $ do
      putInfo $ "Cleaning files in " ++ sbOutDir
      removeFilesAfter sbOutDir ["//*"]

{-|
  Default "shake statics" phony rule. automatically runs need on "\<out\>\/thing\/\*" for every
  thing found in "images\/", "css\/", "js\/" and "webfonts\/"
-}
defaultStaticsPhony :: MonadShakebookRules r m => m ()
defaultStaticsPhony = view sbConfigL >>= \SbConfig {..} -> liftRules $
  phony "statics" $ do
    fp <- getDirectoryFiles sbSrcDir ["images//*", "css//*", "js//*", "webfonts//*"]
    need $ [sbOutDir </> x | x <- fp]

{-|
  Default "shake posts" phony rule. takes a [FilePattern] pointing to the posts and
  and calls need on "\<out\>\/posts\/\<filename\>.html" for each markdown post found.
-}  
defaultPostsPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultPostsPhony pattern = view sbConfigL >>= \SbConfig {..} -> liftRules $
  phony "posts" $ do
    fp <- getDirectoryFiles sbSrcDir pattern
    need [sbOutDir </> x -<.> ".html" | x <- fp]

{-|
  Default "shake posts-index" phony rule. Takes a [FilePattern] of posts to
  discover and calls need on "\<out\>\/posts\/index.html" and
  "\<out\>\/posts\/pages\/\<n\>\/index.html" for each page required.
-}
defaultPostIndexPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultPostIndexPhony pattern = ask >>= \r -> view sbConfigL >>= \SbConfig {..} -> liftRules $
    phony "posts-index" $ do
      fp <- runShakebookA r $ getMarkdown pattern
      need [sbOutDir </> "posts/index.html"]
      need [sbOutDir </> "posts/pages/" ++ show x ++ "/index.html"
           | x <- [1..size (fromJust $ paginate sbPPP fp)]]

{-|
  Default "shake tag-index" phony rule. Takes a [FilePattern] of posts to
  discover and calls need on "\<out\>\/posts\/tags\/\<tag\>\/index.html" and
  "\<out\>\/posts\/tags\/\<tag\>\/pages\/\<n\>\/index.html" for each tag discovered and for
  each page required per tag filter.
-}
defaultTagIndexPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultTagIndexPhony pattern = ask >>= \r -> view sbConfigL >>= \SbConfig {..} -> liftRules $
  phony "tag-index" $ do
    fp <- runShakebookA r $ getMarkdown pattern
    let tags = viewAllPostTags fp
    need [sbOutDir </> "posts/tags" </> T.unpack x </> "index.html" | x <- tags]
    need [sbOutDir </> "posts/tags" </> T.unpack x </> "pages" </> show p </> "index.html"
         |  x <- tags
         ,  p <- [1..size (fromJust $ paginate sbPPP $ tagFilterPosts x fp)]
         ]

{-|
  Default "shake month-index" phony rule. Takes a [FilePattern] of posts to
  discover and calls need on "\<out\>\/posts\/months\/\<yyyy-md\>\/index.html" and
  "\<out\>\/posts\/months\/\<yyyy-md\>\/pages\/\<n\>\/index.html" for each month
  discovered that contains a post and for each page required per month filter.
-}
defaultMonthIndexPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultMonthIndexPhony pattern = ask >>= \r -> view sbConfigL >>= \SbConfig {..} -> liftRules $
   phony "month-index" $ do
      fp <- runShakebookA r $ getMarkdown pattern
      let times = viewAllPostTimes fp
      need [sbOutDir </> "posts/months" </> defaultMonthURLFormat t </> "index.html" | t <- times]
      need [sbOutDir </> "posts/months" </> defaultMonthURLFormat t </> "pages" </> show p </> "index.html"
           | t <- times
           , p <- [1..length (fromJust $ paginate sbPPP $ monthFilterPosts t fp)]
           ]

-- | Default "shake docs" phony rule, takes a Cofree [] String as a table of contents.
defaultDocsPhony :: MonadShakebookRules r m
                 => Cofree [] String
                 -> m ()
defaultDocsPhony toc = view sbConfigL >>= \SbConfig {..} -> liftRules $
    phony "docs" $ do
      let xs =  (foldr ((<>) . pure) [] toc)
      need $ [ (sbOutDir </>) . (-<.> ".html") $ x | x <- xs]
