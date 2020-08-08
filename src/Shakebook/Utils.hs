module Shakebook.Utils where

import           Data.List.Split
import           Path
import           RIO
import qualified RIO.HashMap as HM
import Control.Comonad
import Control.Comonad.Cofree

(</$>) :: Functor f => Path b Dir -> f (Path Rel t) -> f (Path b t)
(</$>) d = fmap (d </>)

changeDir :: MonadThrow m => Path b Dir -> Path b' Dir -> Path b t -> m (Path b' t)
changeDir src dst fp = (dst </>) <$> stripProperPrefix src fp

splitPath :: Path b t -> [String]
splitPath = splitOn "/" . toFilePath

fromCofree :: (Eq a, Hashable a) => Cofree [] a -> HashMap a [a]
fromCofree = HM.fromList . foldr ((<>) . pure) [] . extend (\(x :< xs) -> (x, fmap extract xs))

data KeyNotFoundException a = KeyNotFoundException a
  deriving (Eq, Ord, Show)

instance (Typeable a, Show a) => Exception (KeyNotFoundException a)

lookupOrThrow :: (Eq a, Show a, Typeable a, Hashable a, MonadThrow m) => a -> HashMap a b -> m b
lookupOrThrow k m = case HM.lookup k m of
                          Just x -> return x
                          Nothing -> throwM $ KeyNotFoundException k
