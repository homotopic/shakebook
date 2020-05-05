module Shakebook.Shake (
 getDirectoryFilesWithin'
, needPathIn
, needWithin
, readFileIn'
, putInfo
, (%>~)
, needLocalOut
, loadSortFilterApply
, HasLocalOut(..)
, traverseToSnd
) where

import Control.Exception
import Control.Exception.Extra
import qualified Development.Shake
import qualified Development.Shake.FilePath
import Development.Shake (Action, Rules, FilePattern, RuleResult, ShakeValue)
import Path
import RIO
import RIO.List
import qualified RIO.Text as T
import Shakebook.Within
import Development.Shake.Plus

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd f a = (a,) <$> f a

getDirectoryFilesWithin' :: MonadAction m => Path Rel Dir -> [FilePattern] -> m [Within Rel File]
getDirectoryFilesWithin' x pat = do
  xs <- getDirectoryFiles x pat
  return ((`within` x) <$> xs)

needLocalOut :: (MonadAction m, MonadReader r m, HasLocalOut r) => [Path Rel File] -> m ()
needLocalOut ys = view localOutL >>= \r -> needPathIn r ys

needPathIn :: MonadAction m => Path Rel Dir -> [Path Rel File] -> m ()
needPathIn x ys = liftAction $ needWithin $ map (\y -> Within (x, y)) ys

needWithin :: MonadAction m => [Within Rel File] -> m ()
needWithin = need . map fromWithin

readFileIn' :: MonadAction m => Path Rel Dir -> Path Rel File -> m Text
readFileIn' x y = readFile' $ x </> y

putInfo :: MonadAction m => String -> m ()
putInfo = liftAction . Development.Shake.putInfo

(%>~) :: (MonadReader r m, MonadRules m, HasLocalOut r) => FilePattern -> (Within Rel File -> RAction r ()) -> m ()
(%>~) pat f = view localOutL >>= \o -> (toFilePath o Development.Shake.FilePath.</> pat) %> \x -> (x `asWithin` o) >>= f

loadSortFilterApply :: (MonadAction m, Ord b)
                    => Path Rel Dir
                    -> [FilePattern]
                    -> (Path Rel File -> m a)
                    -> (a -> b)
                    -> (a -> Bool)
                    -> (a -> a)
                    -> m[(Path Rel File, a)]
loadSortFilterApply dir pat l s f e = do
  xs <- getDirectoryFiles dir pat >>= mapM (traverseToSnd l)
  return $ fmap (second e) $ sortOn (s . snd) $ filter (f . snd) $ xs

class HasLocalOut r where
  localOutL :: Lens' r (Path Rel Dir)
