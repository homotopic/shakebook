module Shakebook.Rules where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import qualified Development.Shake as S
import           Development.Shake.Plus
import Path
import           RIO
import           Within

storeRuleGen :: (ComonadStore s w, MonadRules m, MonadReader r m)
             => FilePattern
             -> (Path Rel File -> RAction r (w b))
             -> (w b -> Path Rel File -> RAction r ())
             -> m ()
storeRuleGen fp rgen ract = fp %> liftA2 (>>=) rgen (flip ract)

storeRuleGenWithin :: (ComonadStore s w, MonadRules m, MonadReader r m)
                   => Within Rel FilePattern
                   -> (Within Rel (Path Rel File) -> RAction r (w b))
                   -> (w b -> Within Rel (Path Rel File) -> RAction r ())
                   -> m ()
storeRuleGenWithin fp rgen ract = fp %^> liftA2 (>>=) rgen (flip ract)

storeRuleGenWithinWizard :: (ComonadStore s w, MonadRules m, MonadReader r m)
                         => (Within Rel FilePattern)
                         -> (Within Rel (Path Rel File) -> RAction r s)
                         -> (Within Rel (Path Rel File) -> RAction r a)
                         -> (a -> RAction r (w b))
                         -> (w b -> Within Rel (Path Rel File) -> RAction r ())
                         -> m ()
storeRuleGenWithinWizard fp sgen agen cgen ract = do
  storeRuleGenWithin fp
    (\x -> do
      xs <- (cgen <=< agen) x
      s <- sgen x
      return $ seek s xs)
    ract

{-|
  Generates Shake `Rules` from a FilePattern via an action that returns a `ComonadStore`.
-}
comonadStoreRuleGen :: (ComonadStore s w, MonadRules m, MonadReader r m)
                    => (Within Rel FilePattern) -- ^ The filepattern rule.
                    -> (Within Rel (Path Rel File) -> s) -- ^ How to extract a position marker from the filepattern.
                    -> (Within Rel (Path Rel File) -> a) -- ^ How to extract an id from the filepattern.
                    -> (a -> RAction r (w b)) -- ^ How to turn the id into a searchable store.
                    -> (b -> Within Rel (Path Rel File) -> RAction r ())
                    -> m ()
comonadStoreRuleGen fp f g h k =
  fp %^> \x -> do
    xs <- h (g (x))
    k (extract (seek (f x) xs)) x

{-|
  Generates Shake `Rules` from a `ComonadCofree` of `FilePath` sources.
-}
cofreeRuleGen :: (Traversable w, ComonadCofree f w)
              => w FilePath -- ^ A cofree comonad of FilePaths
              -> (FilePath -> FilePath) -- ^ How to find the out path for each source FilePath.
              -> (w FilePath -> FilePath -> Action ()) -- ^ How to generate a write Action for the target of a comonad. This is extended over the whole comonad.
              -> Rules ()
cofreeRuleGen xs h k = do
  let f ys = h (extract ys) S.%> k ys
  void . sequence . extend f $ xs
