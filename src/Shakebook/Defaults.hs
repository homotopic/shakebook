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
import           RIO                        hiding (view)
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

(<~>) = liftA2 (.)

defaultEnrichPost :: ShakebookA (Value -> Value)
defaultEnrichPost = typicalUrlEnricher 
                <~> pure (enrichTeaser "<!--more-->")
                <~> pure (enrichTagLinks ("/posts/tags/" <>))
                <~> pure (enrichPrettyDate prettyTimeFormat)

typicalMarkdownPath = pure (-<.> "md") <~> typicalSourcePath

typicalSourcePath :: MonadReader SbConfig m => m (String -> String)
typicalSourcePath = ask >>= \SbConfig{..} -> pure $
   ((sbSrcDir </>) . drop 1 . fromJust . stripPrefix sbOutDir)

typicalHTMLPath :: MonadReader SbConfig m => m (String -> String)
typicalHTMLPath = ask >>= \SbConfig{..} -> pure $
  (-<.> "html") . (sbOutDir </>) . drop 1 . fromJust . stripPrefix sbSrcDir
  
typicalUrlTransformer :: MonadReader SbConfig m => m (Text -> Text)
typicalUrlTransformer = ask >>= \SbConfig{..} -> pure $
  T.pack . (-<.> "html") . fromJust . stripPrefix sbSrcDir . T.unpack

typicalUrlEnricher :: MonadReader SbConfig m => m (Value -> Value)
typicalUrlEnricher = typicalUrlTransformer >>= \e -> pure (withUrl =<< e . viewSrcPath)

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
affixBlogNavbar :: [FilePattern] -> Text -> Text -> (UTCTime -> Text) -> (UTCTime -> Text) -> ShakebookA (Value -> Value)-> Value -> ShakebookA Value
affixBlogNavbar patterns a b c d e x = do
  xs <- loadSortEnrich patterns (Down . viewPostTime) e
  return $ withJSON (genBlogNavbarData a b c d (snd <$> xs)) $ x


affixRecentPosts :: [FilePattern] -> Int -> ShakebookA (Value -> Value) -> Value -> ShakebookA Value
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

defaultDocsPatterns :: Cofree [] FilePath -- Rosetree Table of Contents.
                    -> FilePath
                    -> (Value -> Value) -- Extra data modifiers.
                    -> Shakebook ()
defaultDocsPatterns toc tmpl withData = ask >>= \SbConfig {..} -> do
  let r = readMarkdownFile' sbMdRead sbHTWrite
  e <- typicalUrlEnricher
  m <- typicalMarkdownPath
  Shakebook $ lift $ cofreeRuleGen toc ((sbOutDir </>) . (-<.> ".html")) (
         \xs -> \out -> do 
             ys <- mapM r (fmap (sbSrcDir </>) toc)
             zs <- mapM r (fmap (sbSrcDir </>) xs)
             void $ genBuildPageAction (sbSrcDir </> tmpl)
                      (r . m)
                      (withData
                     . withJSON (genTocNavbarData (e <$> ys))
                     . withSubsections (lower (e <$> zs)))
                      out)

defaultPostIndexData :: [FilePattern] -> (a -> Value -> Bool) -> (a -> Text) -> (a -> Text -> Text) -> a -> ShakebookA (Zipper [] Value)
defaultPostIndexData pat f t l a = ask >>= \SbConfig {..} -> do
  xs <- loadSortFilterEnrich pat (Down . viewPostTime) (f a) defaultEnrichPost
  let ys = genIndexPageData (snd <$> xs) (t a) (l a) sbPPP
  return $ fromJust $ ys

defaultPagerPattern :: FilePattern
                    -> FilePath
                    -> (FilePattern -> Int)
                    -> (FilePattern -> a)
                    -> (a -> ShakebookA (Zipper [] Value))
                    -> (Zipper [] Value -> ShakebookA (Zipper [] Value))
                    -> Shakebook ()
defaultPagerPattern fp tmpl f g h w = Shakebook $ ask >>= \x@SbConfig{..} -> lift $
  comonadStoreRuleGen (sbOutDir </> fp) (f . drop 1 . fromJust . stripPrefix sbOutDir) (g . drop 1 . fromJust . stripPrefix sbOutDir) (runShakebookA x . (w <=< h))
  (\a -> void <$> genBuildPageAction (sbSrcDir </> tmpl) (const $ return a) id)

defaultPostIndexPatterns :: [FilePattern] -> FilePath -> (Zipper [] Value -> ShakebookA (Zipper [] Value)) -> Shakebook ()
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

defaultTagIndexPatterns :: [FilePattern] -> FilePath -> (Zipper [] Value -> ShakebookA (Zipper [] Value)) -> Shakebook ()
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

defaultMonthIndexPatterns :: [FilePattern] -> FilePath -> (Zipper [] Value -> ShakebookA (Zipper [] Value)) -> Shakebook ()
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

defaultPostsPatterns :: FilePattern
                     -> FilePath 
                     -> (Value -> ShakebookA Value)
                     -> (Zipper [] Value -> ShakebookA (Zipper [] Value))
                     -> Shakebook ()
defaultPostsPatterns pat tmpl e extData = ask >>= \sbc@(SbConfig {..}) -> do
  m <- typicalMarkdownPath
  Shakebook $ lift $ do
    sbOutDir </> pat %> \out -> do
      sortedPosts <- runShakebookA sbc $ do
        xs <-loadSortEnrich [pat] (Down . viewPostTime) (pure id)
        mapM (\(s,x) -> e x >>= \e' -> return (s, e')) xs
      let i = (-<.> ".md") . drop 1 . fromJust . stripPrefix sbOutDir $ out
      let k = fromJust $ elemIndex i (fst <$> sortedPosts)
      let z = fromJust $ seek k <$> zipper (snd <$> sortedPosts)
      void $ genBuildPageAction (sbSrcDir </> tmpl)
                                (const $ runShakebookA sbc $ extract <$> extData z)
                                id out


buildPDF :: Cofree [] String -> String -> FilePath -> ShakebookA ()
buildPDF toc meta out = ShakebookA $ ask >>= \SbConfig {..} -> lift $ do
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

defaultSinglePagePattern :: FilePath -- The output filename e.g "index.html".
                         -> FilePath -- A tmpl file.
                         -> (Value -> ShakebookA Value) -- Last minute enrichment.
                         -> Shakebook ()
defaultSinglePagePattern out tmpl withDataM = ask >>= \sbc@(SbConfig {..}) -> do
  m <- typicalMarkdownPath
  e <- typicalUrlEnricher
  Shakebook $ lift $ sbOutDir </> out %> void . genBuildPageAction
                 (sbSrcDir </> tmpl)
                 (\fp -> do
                   x <- readMarkdownFile' sbMdRead sbHTWrite . m $ fp
                   runShakebookA sbc $ withDataM . e $ x)
                 id

defaultStaticsPatterns :: [FilePattern] -> Shakebook ()
defaultStaticsPatterns xs = ask >>= \SbConfig {..} -> do
  f <- typicalSourcePath
  Shakebook $ lift $ mconcat $ map (\x -> sbOutDir </> x %> \y -> copyFileChanged (f y) y) xs

defaultCleanPhony :: Shakebook ()
defaultCleanPhony = Shakebook $ ask >>= \SbConfig {..} -> lift $
  phony "clean" $ do
      putInfo $ "Cleaning files in " ++ sbOutDir
      removeFilesAfter sbOutDir ["//*"]

defaultStaticsPhony :: Shakebook ()
defaultStaticsPhony = Shakebook $ ask >>= \SbConfig {..} -> lift $
  phony "statics" $ do
    fp <- getDirectoryFiles sbSrcDir ["images//*", "css//*", "js//*", "webfonts//*"]
    need $ [sbOutDir </> x | x <- fp]

defaultPostsPhony :: [FilePattern] -> Shakebook ()
defaultPostsPhony pattern = Shakebook $ ask >>= \SbConfig {..} -> lift $
  phony "posts" $ do
    fp <- getDirectoryFiles sbSrcDir pattern
    need [sbOutDir </> x -<.> ".html" | x <- fp]

defaultPostIndexPhony :: [FilePattern] -> Shakebook ()
defaultPostIndexPhony pattern = Shakebook $ ask >>= \SbConfig {..} -> lift $
    phony "posts-index" $ do
      fp <- getDirectoryMarkdown sbMdRead sbHTWrite sbSrcDir pattern
      need [sbOutDir </> "posts/index.html"]
      need [sbOutDir </> "posts/pages/" ++ show x ++ "/index.html"
           | x <- [1..size (fromJust $ paginate sbPPP fp)]]

defaultTagIndexPhony :: [FilePattern] -> Shakebook ()
defaultTagIndexPhony pattern = Shakebook $ ask >>= \SbConfig {..} -> lift $
  phony "tag-index" $ do
    fp <- getDirectoryMarkdown sbMdRead sbHTWrite sbSrcDir pattern
    let tags = viewAllPostTags fp
    need [sbOutDir </> "posts/tags" </> T.unpack x </> "index.html" | x <- tags]
    need [sbOutDir </> "posts/tags" </> T.unpack x </> "pages" </> show p </> "index.html"
         |  x <- tags
         ,  p <- [1..size (fromJust $ paginate sbPPP $ tagFilterPosts x fp)]
         ]

defaultMonthIndexPhony :: [FilePattern] -> Shakebook ()
defaultMonthIndexPhony pattern = Shakebook $ ask >>= \SbConfig {..} -> lift $
   phony "month-index" $ do
      fp <- getDirectoryMarkdown sbMdRead sbHTWrite sbSrcDir pattern
      let times = viewAllPostTimes fp
      need [sbOutDir </> "posts/months" </> monthURLFormat t </> "index.html" | t <- times]
      need [sbOutDir </> "posts/months" </> monthURLFormat t </> "pages" </> show p </> "index.html"
           | t <- times
           , p <- [1..length (fromJust $ paginate sbPPP $ monthFilterPosts t fp)]
           ]

defaultDocsPhony :: Cofree [] String -> Shakebook ()
defaultDocsPhony toc = Shakebook $ ask >>= \SbConfig {..} -> lift $
    phony "docs" $ do
      let xs =  (foldr ((<>) . pure) [] toc)
      need $ [ (sbOutDir </>) . (-<.> ".html") $ x | x <- xs]
