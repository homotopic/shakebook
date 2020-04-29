{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Shakebook.Defaults where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Store.Class
import           Control.Comonad.Store.Zipper
import           Control.Monad.Extra
import           Data.Aeson                 as A
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake          as S
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           RIO                        
import qualified RIO.ByteString.Lazy        as LBS
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Map                    as M
import           RIO.Partial
import qualified RIO.Text                   as T
import           RIO.Time
import           Shakebook.Aeson
import           Shakebook.Data
import           Shakebook.Rules
import           Shakebook.Conventions
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

monthURLFormat :: UTCTime -> String
monthURLFormat = formatTime defaultTimeLocale "%Y-%m"

prettyMonthFormat :: UTCTime -> String
prettyMonthFormat = formatTime defaultTimeLocale "%B, %Y"

prettyTimeFormat :: UTCTime -> String
prettyTimeFormat = formatTime defaultTimeLocale "%A, %B %d, %Y"

monthIndexUrlFormat :: UTCTime -> String
monthIndexUrlFormat t = "/posts/months" </> monthURLFormat t

defaultEnrichPost :: Value -> Value
defaultEnrichPost = enrichTeaser "<!--more-->"
                  . enrichTagLinks ("/posts/tags/" <>)
                  . enrichPrettyDate prettyTimeFormat

--Data models-------------------------------------------------------------------

markdownReaderOptions :: ReaderOptions
markdownReaderOptions = def { readerExtensions = pandocExtensions }

html5WriterOptions :: WriterOptions
html5WriterOptions = def { writerHTMLMathMethod = MathJax ""}

latexWriterOptions :: WriterOptions
latexWriterOptions = def { writerTableOfContents = True
                         , writerVariables = Context $ M.fromList [
                                               ("geometry", SimpleVal "margin=3cm")
                                             , ("fontsize", SimpleVal "10")
                                             , ("linkcolor",SimpleVal "blue")]
                         }
affixBlogNavbar :: MonadShakebookAction r m
                => [FilePattern]
                -> Text
                -> Text
                -> (UTCTime -> Text)
                -> (UTCTime -> Text)
                -> (Value -> Value)
                -> Value -> m Value
affixBlogNavbar patterns a b c d e x = do
  xs <- loadSortEnrich patterns (Down . viewPostTime) e
  return $ withJSON (genBlogNavbarData a b c d (snd <$> xs)) $ x

affixRecentPosts :: MonadShakebookAction r m
                 => [FilePattern]
                 -> Int
                 -> (Value -> Value)
                 -> Value -> m Value
affixRecentPosts patterns n e x = do
  xs <- loadSortEnrich patterns (Down . viewPostTime) e
  return $ withRecentPosts (take n (snd <$> xs)) $ x

makePDFLaTeX :: Pandoc -> PandocIO (Either LBS.ByteString LBS.ByteString)
makePDFLaTeX p = do
  t <- compileDefaultTemplate "latex"
  makePDF "pdflatex" [] writeLaTeX latexWriterOptions { writerTemplate = Just t } p

handleImages :: (Text -> Text) -> Inline -> Inline
handleImages f (Image attr ins (src,txt)) =
  if T.takeEnd 4 src == ".mp4" then Str (f src)
  else Image attr ins ("public/" <> src, txt)
handleImages _ x = x

handleHeaders :: Int -> Block -> Block
handleHeaders i (Header a as xs) = Header (max 1 (a + i)) as xs
handleHeaders _ x                  = x

pushHeaders :: Int -> Cofree [] Pandoc -> Cofree [] Pandoc
pushHeaders i (x :< xs) = walk (handleHeaders i) x :< map (pushHeaders (i+1)) xs

defaultDocsPatterns :: MonadShakebookRules r m
                    => Cofree [] FilePath -- Rosetree Table of Contents.
                    -> FilePath
                    -> (Value -> Value) -- Extra data modifiers.
                    -> m ()
defaultDocsPatterns toc tmpl withData = ask >>= \r -> view sbConfigL >>= \sbc@SbConfig {..} -> do
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
                     -> (a -> Value -> Bool)
                     -> (a -> Text)
                     -> (a -> Text -> Text)
                     -> a
                     -> m (Zipper [] Value)
defaultPostIndexData pat f t l a = view sbConfigL >>= \SbConfig {..} -> do
  xs <- loadSortFilterEnrich pat (Down . viewPostTime) (f a) defaultEnrichPost
  let ys = genIndexPageData (snd <$> xs) (t a) (l a) sbPPP
  return $ fromJust $ ys

defaultPagerPattern :: MonadShakebookRules r m
                    => FilePattern
                    -> FilePath
                    -> (FilePattern -> Int)
                    -> (FilePattern -> a)
                    -> (a -> ShakebookA r (Zipper [] Value))
                    -> (Zipper [] Value -> ShakebookA r (Zipper [] Value))
                    -> m ()
defaultPagerPattern fp tmpl f g h w = ask >>= \r -> view sbConfigL >>= \x@SbConfig{..} -> liftRules $
  comonadStoreRuleGen (sbOutDir </> fp) (f . drop 1 . fromJust . stripPrefix sbOutDir) (g . drop 1 . fromJust . stripPrefix sbOutDir) (runShakebookA r . (w <=< h))
  (\a -> void <$> runShakebookA r . genBuildPageAction (sbSrcDir </> tmpl) (const $ return a) id)

defaultPostIndexPatterns :: MonadShakebookRules r m => [FilePattern] -> FilePath -> (Zipper [] Value -> ShakebookA r (Zipper [] Value)) -> m ()
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

defaultTagIndexPatterns :: MonadShakebookRules r m => [FilePattern] -> FilePath -> (Zipper [] Value -> ShakebookA r (Zipper [] Value)) -> m ()
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

defaultMonthIndexPatterns :: MonadShakebookRules r m => [FilePattern] -> FilePath -> (Zipper [] Value -> ShakebookA r (Zipper [] Value)) -> m ()
defaultMonthIndexPatterns pat tmpl extData = do
 defaultPagerPattern "posts/months/*/index.html" tmpl
                     (const 0)
                     (parseISODateTime . T.pack . (!! 2) . splitOn "/")
                     (defaultPostIndexData pat
                        (\x y -> sameMonth x (viewPostTime y))
                        (("Posts from " <>) . T.pack . prettyMonthFormat)
                        (\x y -> ("/posts/months/" <> T.pack (monthURLFormat x) <> "/pages" <> y)))
                     extData
 defaultPagerPattern "posts/months/*/pages/*/index.html" tmpl
                      ((+ (-1)) . read . (!! 4) . splitOn "/")
                      (parseISODateTime . T.pack . (!! 2) . splitOn "/")
                       (defaultPostIndexData pat
                           (\x y -> sameMonth x (viewPostTime y))
                           (("Posts from " <>) . T.pack . prettyMonthFormat)
                           (\x y -> ("/posts/months/" <> T.pack (monthURLFormat x) <> "/pages" <> y)))
                       extData

defaultPostsPatterns :: MonadShakebookRules r m
                     => FilePattern
                     -> FilePath 
                     -> (Value -> ShakebookA r Value)
                     -> (Zipper [] Value -> ShakebookA r (Zipper [] Value))
                     -> m ()
defaultPostsPatterns pat tmpl e extData = ask >>= \r -> view sbConfigL >>= \sbc@(SbConfig {..}) -> do
  m <- typicalFullOutHTMLToMdSrcPath
  liftRules $ do
    sbOutDir </> pat %> \out -> do
      sortedPosts <- runShakebookA r $ do
        xs <- loadSortEnrich [pat] (Down . viewPostTime) defaultEnrichPost
        mapM (\(s,x) -> e x >>= \e' -> return (s, e')) xs
      let i = (-<.> ".md") . drop 1 . fromJust . stripPrefix sbOutDir $ out
      let k = fromJust $ elemIndex i (fst <$> sortedPosts)
      let z = fromJust $ seek k <$> zipper (snd <$> sortedPosts)
      void $ runShakebookA r $ genBuildPageAction (sbSrcDir </> tmpl)
                                (const $ extract <$> extData z)
                                id out


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

flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff

defaultSinglePagePattern :: MonadShakebookRules r m
                         => FilePath -- The output filename e.g "index.html".
                         -> FilePath -- A tmpl file.
                         -> (Value -> ShakebookA r Value) -- Last minute enrichment.
                         -> m ()
defaultSinglePagePattern out tmpl withDataM = ask >>= \r -> view sbConfigL >>= \sbc@(SbConfig {..}) -> do
  m <- typicalFullOutHTMLToMdSrcPath
  liftRules $ sbOutDir </> out %> void . runShakebookA r . genBuildPageAction
                 tmpl
                 (\fp -> do
                    x <- readMarkdownFile' . m $ fp
                    withDataM $ x)
                 id

defaultStaticsPatterns :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultStaticsPatterns xs = view sbConfigL >>= \SbConfig {..} -> do
  f <- typicalFullOutToSrcPath
  liftRules $ mconcat $ map (\x -> sbOutDir </> x %> \y -> copyFileChanged (f y) y) xs

defaultCleanPhony :: MonadShakebookRules r m => m ()
defaultCleanPhony = view sbConfigL >>= \SbConfig {..} -> liftRules $ 
  phony "clean" $ do
      putInfo $ "Cleaning files in " ++ sbOutDir
      removeFilesAfter sbOutDir ["//*"]

defaultStaticsPhony :: MonadShakebookRules r m => m ()
defaultStaticsPhony = view sbConfigL >>= \SbConfig {..} -> liftRules $ 
  phony "statics" $ do
    fp <- getDirectoryFiles sbSrcDir ["images//*", "css//*", "js//*", "webfonts//*"]
    need $ [sbOutDir </> x | x <- fp]

defaultPostsPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultPostsPhony pattern = view sbConfigL >>= \SbConfig {..} -> liftRules $ 
  phony "posts" $ do
    fp <- getDirectoryFiles sbSrcDir pattern
    need [sbOutDir </> x -<.> ".html" | x <- fp]

defaultPostIndexPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultPostIndexPhony pattern = ask >>= \r -> view sbConfigL >>= \SbConfig {..} -> liftRules $
    phony "posts-index" $ do
      fp <- runShakebookA r $ getMarkdown pattern
      need [sbOutDir </> "posts/index.html"]
      need [sbOutDir </> "posts/pages/" ++ show x ++ "/index.html"
           | x <- [1..size (fromJust $ paginate sbPPP fp)]]

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

defaultMonthIndexPhony :: MonadShakebookRules r m => [FilePattern] -> m ()
defaultMonthIndexPhony pattern = ask >>= \r -> view sbConfigL >>= \SbConfig {..} -> liftRules $
   phony "month-index" $ do
      fp <- runShakebookA r $ getMarkdown pattern
      let times = viewAllPostTimes fp
      need [sbOutDir </> "posts/months" </> monthURLFormat t </> "index.html" | t <- times]
      need [sbOutDir </> "posts/months" </> monthURLFormat t </> "pages" </> show p </> "index.html"
           | t <- times
           , p <- [1..length (fromJust $ paginate sbPPP $ monthFilterPosts t fp)]
           ]

defaultDocsPhony :: MonadShakebookRules r m => Cofree [] String -> m ()
defaultDocsPhony toc = view sbConfigL >>= \SbConfig {..} -> liftRules $
    phony "docs" $ do
      let xs =  (foldr ((<>) . pure) [] toc)
      need $ [ (sbOutDir </>) . (-<.> ".html") $ x | x <- xs]
