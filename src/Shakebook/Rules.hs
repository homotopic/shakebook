module Shakebook.Rules where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           RIO
import           Development.Shake

comonadStoreRuleGen :: ComonadStore s w
                    => FilePattern -- The filepattern rule.
                    -> (FilePattern -> s) -- How to extract a position marker from the filepattern.
                    -> (FilePattern -> a) -- How to extract an id from the filepattern.
                    -> (a -> Action (w b)) -- How to turn the id into a searchable store.
                    -> (b -> FilePath -> Action ())
                    -> Rules ()
comonadStoreRuleGen fp f g h k = do
  fp %> \x -> do
    xs <- h (g x)
    k (extract (seek (f x) xs)) x


cofreeRuleGen :: (Traversable w, ComonadCofree f w)
              => w FilePath
              -> (FilePath -> FilePath)
              -> (w FilePath -> FilePath -> Action ())
              -> Rules ()
cofreeRuleGen xs h k = do
  let f ys = h (extract ys) %> k ys
  void . sequence . extend f $ xs
