module Shakebook.Utils where

import           Composite.Record
import           Control.Comonad
import           Control.Comonad.Cofree
import           Data.List.Split
import           Data.Vinyl
import           Data.Vinyl.TypeLevel
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

rmapToSnd :: (a -> b) -> a -> Record (s :-> a : s' :-> b : '[])
rmapToSnd f x = x :*: f x :*: RNil

rmapToFst :: (a -> b) -> a -> Record (s :-> b : s' :-> a : '[])
rmapToFst f x = f x :*: x :*: RNil

rmapToPush :: (Record xs -> a) -> Record xs -> Record (s :-> a : xs)
rmapToPush f xs = f xs :*: xs

rmapToPrepend :: (Record xs -> Record ys) -> Record xs -> Record (ys ++ xs)
rmapToPrepend f xs = f xs <+> xs

rtraverseToFst :: Monad m => (a -> m b) -> a -> m (Record (s :-> b : s' :-> a : '[]))
rtraverseToFst f x = f x >>= \y -> return $ y :*: x :*: RNil

rtraverseToSnd :: Monad m => (a -> m b) -> a -> m (Record (s :-> a : s' :-> b : '[]))
rtraverseToSnd f x =  f x >>= \y -> return $ x :*: y :*: RNil

rtraverseToPush :: Monad m => (Record xs -> m a) -> Record xs -> m (Record (s :-> a : xs))
rtraverseToPush f xs = f xs >>= \y -> return $ y :*: xs

rfanout :: (x -> a) -> (x -> b) -> x -> Record (s :-> a : s' :-> b : '[])
rfanout f g x = f x :*: g x :*: RNil

rfanoutM :: Monad m => (x -> m a) -> (x -> m b) -> x -> m (Record (s :-> a : s' :-> b : '[]))
rfanoutM f g x = do
  y <- f x
  z <- g x
  return $ y :*: z :*: RNil
