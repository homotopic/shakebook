{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Comonad.Env as E
import           Development.Shake.Plus
import           Options.Applicative
import           Path.Extensions
import           RIO
import qualified RIO.HashMap as HM
import           RIO.List
import qualified RIO.Text                   as T
import           Shakebook
import           Text.Pandoc.Options

sample :: Parser SimpleOpts
sample = SimpleOpts
      <$> strOption ( long "src" <> metavar "SOURCEDIR" <> help "The source directory."
                    <> showDefault <> value "site" )
      <*> strOption ( long "out" <> metavar "OUTPUTDIR" <> help "The output directory."
                    <> showDefault <> value "public")
      <*> strOption ( long "baseUrl" <> metavar "BASEURL" <> help "The base url for your site.")
      <*> option auto ( long "ppp" <> metavar "POSTSPERPAGE" <> help "Num of posts per page.")


data SimpleOpts = SimpleOpts
    { srcDir  :: String
    , outDir  :: String
    , baseUrl :: String
    , ppp     :: Int
    }

data BuildOpts = BuildOpts
    { sbSrcDir :: Path Rel Dir
    , sbOutDir :: Path Rel Dir
    , sbBaseUrl :: Text
    , sbMdRead :: ReaderOptions
    , sbHtWrite :: WriterOptions
    , sbPPP :: Int
    }
data AppConfig = AppConfig
    { _opts :: BuildOpts
    , _lf   :: LogFunc
    }

class HasBuildOpts a where
  buildOptsL :: Lens' a BuildOpts

instance HasLogFunc AppConfig where
  logFuncL = lens _lf undefined

instance HasBuildOpts AppConfig where
  buildOptsL = lens _opts undefined

opts :: ParserInfo SimpleOpts
opts = info (sample <**> helper)
    ( fullDesc
   <> progDesc "Creates a simple blog from source with default settings."
   <> header "shakebook-simple-blog - A simple blog using standard shakebook conventions." )

main :: IO ()
main = do
  (x :: SimpleOpts) <- execParser opts
  s' <- parseRelDir (srcDir x)
  o' <- parseRelDir (outDir x)
  app $ BuildOpts s' o' (T.pack $ baseUrl x) defaultMarkdownReaderOptions defaultHtml5WriterOptions (ppp x)

app :: BuildOpts -> IO ()
app sbc =  do
    logOptions' <- logOptionsHandle stdout True
    lf <- newLogFunc logOptions'
    let f = AppConfig sbc (fst lf)

    shake shakeOptions $ do

      want ["all"]

      runShakePlus f $ do
        BuildOpts{..} <- view buildOptsL

        readMDC <- newCache $ loadMarkdownAsJSON sbMdRead sbHtWrite

        postsC  <- newCache $ \w -> do
          xs <- batchLoadWithin' w readMDC
          return $ defaultEnrichPost <$> xs

        getRecentPosts <- newCache $ \fp -> do
          allPosts <- postsC fp
          return $ take 5 (sortOn (Down . viewPostTime) $ HM.elems allPosts)

        ("index.html" `within` sbOutDir) %^> \out -> do
          src <- blinkAndMapM sbSrcDir withMdExtension out
          v   <- readMDC src
          r   <- getRecentPosts (["posts/*.md"] `within` sbSrcDir)
          let v' = withRecentPosts r v
          buildPageAction (sbSrcDir </> $(mkRelFile "template/index.html")) v' (fromWithin out)

        phony "index" $ needP [sbOutDir </> $(mkRelFile "index.html")]

        phony "all" $ need ["index"]

    snd lf
