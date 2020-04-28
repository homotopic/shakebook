module Main where

import RIO
import qualified RIO.Text as T
import Development.Shake
import Development.Shake.FilePath
import Shakebook
import Options.Applicative
import Shakebook.Defaults

sample :: Parser SimpleOpts
sample = SimpleOpts
      <$> strOption ( long "src" <> metavar "SOURCEDIR" <> help "The source directory."
                    <> showDefault <> value "site" )
      <*> strOption ( long "out" <> metavar "OUTPUTDIR" <> help "The output directory."
                    <> showDefault <> value "public")
      <*> strOption ( long "baseUrl" <> metavar "BASEURL" <> help "The base url for your site.")
      <*> option auto ( long "ppp" <> metavar "POSTSPERPAGE" <> help "Num of posts per page.")
       

data SimpleOpts = SimpleOpts {
  srcDir :: String
, outDir :: String
, baseUrl :: String
, ppp :: Int
}

opts = info (sample <**> helper)
    ( fullDesc
   <> progDesc "Creates a simple blog from source with default settings."
   <> header "shakebook-simple-blog - A simple blog using standard shakebook conventions." )

main = do
  (x :: SimpleOpts) <- execParser opts

  app $ SbConfig (srcDir x) (outDir x) (T.pack $ baseUrl x) markdownReaderOptions html5WriterOptions (ppp x)

app :: SbConfig -> IO ()
app sbc = shake (shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"] }) $ do

    want ["all"]

    runShakebook sbc $ ask >>= \SbConfig {..} -> do

      defaultCleanPhony

      defaultSinglePagePattern "index.html" "templates/index.html"
                               (affixRecentPosts ["posts/md"] 5 defaultEnrichPost)

      Shakebook $ lift $ phony "index" $ need [sbOutDir </> "index.html"]

    phony "all" $ need ["index"]
