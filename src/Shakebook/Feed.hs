{- |
   Module     : Shakebook.Feed
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Utilities from "Text.Atom.Feed" lifted to `MonadAction` and `FileLike`.
-}
module Shakebook.Feed (
  module Text.Atom.Feed
, buildFeed
) where

import           Development.Shake.Plus
import           RIO
import           RIO.List.Partial
import qualified RIO.Text.Lazy          as LT
import           Text.Atom.Feed
import           Text.Atom.Feed.Export

-- | Build an Atom Feed from a title, a baseUrl and a list of entries.
buildFeed :: (MonadAction m, FileLike b a) => Text -> Text -> [Entry] -> a -> m ()
buildFeed title baseUrl xs out = do
  let t = nullFeed baseUrl (TextString title) $ entryUpdated (head xs)
  case textFeed (t { feedEntries = xs }) of
    Just a  -> writeFile' out $ LT.toStrict a
    Nothing -> return ()
