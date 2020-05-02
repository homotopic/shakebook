module Main where

import           Development.Shake
import           Development.Shake.FilePath as S
import           Options.Applicative
import           Path
import           RIO
import qualified RIO.Text                   as T
import           Shakebook
import           Shakebook.Defaults

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
  app $ SbConfig s' o' (T.pack $ baseUrl x) defaultMarkdownReaderOptions defaultHtml5WriterOptions (ppp x)

app :: SbConfig -> IO ()
app sbc =  do
    logOptions' <- logOptionsHandle stdout True
    lf <- newLogFunc logOptions'
    let f = ShakebookEnv (fst lf) sbc

    shake (shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"] }) $ do

      want ["all"]

      runShakebook f $ view sbConfigL >>= \SbConfig {..} -> do

        defaultCleanPhony

        defaultSinglePagePattern "index.html" "templates/index.html"
                                 (affixRecentPosts ["posts/md"] 5 defaultEnrichPost)

        liftRules $ phony "index" $ need [toFilePath sbOutDir S.</> "index.html"]

      phony "all" $ need ["index"]

    snd lf
