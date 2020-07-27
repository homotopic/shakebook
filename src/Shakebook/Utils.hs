module Shakebook.Utils where

import RIO
import Path
import Data.List.Split

(</$>) :: Functor f => Path b Dir -> f (Path Rel t) -> f (Path b t)
(</$>) d xs = fmap (d </>) xs

changeDir :: MonadThrow m => Path b Dir -> Path b' Dir -> Path b t -> m (Path b' t)
changeDir src dst fp = (dst </>) <$> stripProperPrefix src fp

splitPath :: Path b t -> [String]
splitPath = splitOn "/" . toFilePath
