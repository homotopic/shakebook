{- |
   Module     : Shakebook.Feed
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Utilities from "Text.Atom.Feed" lifted to `MonadAction` and `FileLike`.
-}
{-# LANGUAGE TypeApplications #-}
module Shakebook.Feed 
 where

import           Composite.Record 
import           Development.Shake.Plus hiding ((:->))
import           RIO
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Text               as T
import qualified RIO.Text.Lazy          as TL
import           RIO.Time
import           Shakebook.Conventions
import           Text.Atom.Feed         as Atom
import           Text.Atom.Feed.Export

-- | Convert a Post to an Atom Entry
asAtomEntry :: (RElem FContent xs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs) => Record xs -> Atom.Entry
asAtomEntry x = (Atom.nullEntry
                  (view (rlens (Proxy @FUrl)) x)
                  (Atom.TextString $ view (rlens (Proxy :: Proxy FTitle)) x)
                  (T.pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ view (rlens (Proxy :: Proxy FPosted)) x)) {
                    Atom.entryContent = Just $ Atom.TextContent (view (rlens (Proxy :: Proxy FContent)) x)
  }

-- | Build an Atom Feed from a list of posts.
buildFeed :: (MonadAction m, FileLike b a) => Text -> Text -> [Record '[FContent, FPosted, FTitle, FUrl]] -> a -> m ()
buildFeed title baseUrl xs out = do
  let fs = asAtomEntry <$> sortOn (Down . view (rlens (Proxy :: Proxy FPosted))) xs
  let t = Atom.nullFeed baseUrl (Atom.TextString title) $ Atom.entryUpdated (head fs)
  case  textFeed (t { Atom.feedEntries = fs }) of
    Just a  -> writeFile' out $ TL.toStrict a
    Nothing -> return ()
