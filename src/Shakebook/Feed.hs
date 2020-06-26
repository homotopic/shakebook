module Shakebook.Feed (
  asAtomEntry
, buildFeed
) where

import           Data.Aeson
import           Development.Shake.Plus
import           RIO
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Text.Lazy          as TL
import           Shakebook.Conventions
import           Shakebook.Pandoc
import           Text.Atom.Feed         as Atom
import           Text.Atom.Feed.Export

-- | Convert a Post to an Atom Entry
asAtomEntry :: Value -> Atom.Entry
asAtomEntry x = (Atom.nullEntry (viewUrl x) (Atom.TextString $ viewTitle x) (viewPostTimeRaw x)) {
                       Atom.entryContent = Just $ Atom.TextContent (viewContent x) }

-- | Build an Atom Feed from a list of posts.
buildFeed :: MonadAction m => Text -> Text -> [Value] -> Path Rel File -> m ()
buildFeed title baseUrl xs out = do
  let fs = asAtomEntry <$> sortOn (Down . viewPostTime) xs
  let t = Atom.nullFeed baseUrl (Atom.TextString title) $ Atom.entryUpdated (head fs)
  case  textFeed (t { Atom.feedEntries = fs }) of
    Just a  -> writeFile' out $ TL.toStrict a
    Nothing -> return ()
