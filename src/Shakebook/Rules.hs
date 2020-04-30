module Shakebook.Rules where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Development.Shake
import           RIO

{-|
  Generates Shake `Rules` from a FilePattern via an action that returns a `ComonadStore`.
-}
comonadStoreRuleGen :: ComonadStore s w
                    => FilePattern -- ^ The filepattern rule.
                    -> (FilePattern -> s) -- ^ How to extract a position marker from the filepattern.
                    -> (FilePattern -> a) -- ^ How to extract an id from the filepattern.
                    -> (a -> Action (w b)) -- ^ How to turn the id into a searchable store.
                    -> (b -> FilePath -> Action ())
                    -> Rules ()
comonadStoreRuleGen fp f g h k =
  fp %> \x -> do
    xs <- h (g x)
    k (extract (seek (f x) xs)) x

{-|
  Generates Shake `Rules` from a `ComonadCofree` of `FilePath` sources.
-}
cofreeRuleGen :: (Traversable w, ComonadCofree f w)
              => w FilePath -- ^ A cofree comonad of FilePaths
              -> (FilePath -> FilePath) -- ^ How to find the source for each out FilePath.
              -> (w FilePath -> FilePath -> Action ()) -- ^ How to generate a write Action for the target of a comonad. This is extended over the whole comonad.
              -> Rules ()
cofreeRuleGen xs h k = do
  let f ys = h (extract ys) %> k ys
  void . sequence . extend f $ xs
