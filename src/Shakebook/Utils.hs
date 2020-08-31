module Shakebook.Utils where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Data.List.Split
import           Path
import           RIO
import qualified RIO.HashMap            as HM

(</$>) :: Functor f => Path b Dir -> f (Path Rel t) -> f (Path b t)
(</$>) d = fmap (d </>)

changeDir :: MonadThrow m => Path b Dir -> Path b' Dir -> Path b t -> m (Path b' t)
changeDir src dst fp = (dst </>) <$> stripProperPrefix src fp

splitPath :: Path b t -> [String]
splitPath = splitOn "/" . toFilePath

fromCofree :: (Eq a, Hashable a) => Cofree [] a -> HashMap a [a]
fromCofree = HM.fromList . foldr ((<>) . pure) [] . extend (\(x :< xs) -> (x, fmap extract xs))
