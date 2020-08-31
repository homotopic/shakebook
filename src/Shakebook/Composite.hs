module Shakebook.Composite where

import Composite.Record
import Data.Vinyl hiding (RElem)
import Data.Vinyl.XRec
import Data.Vinyl.TypeLevel
import RIO

toFst :: (a -> b) -> a -> Record (s :-> b : s' :-> a : '[])
toFst f x = f x :*: x :*: RNil

toSnd :: (a -> b) -> a -> Record (s :-> a : s' :-> b : '[])
toSnd f x = x :*: f x :*: RNil

fmapToFst :: (a -> b) -> f a -> f (Record (s :-> a, s' :-> b, : '[]))
fmapToFst :: (a -> b) -> f a -> f (Record (s :-> a, s' :-> b, : '[]))

fmapToSnd :: (a -> b) -> f a -> f (Record (s :-> a, s' :-> b, : '[]))
fmapToSnd :: (a -> b) -> f a -> f (Record (s :-> a, s' :-> b, : '[]))

traverseToFst :: Applicative m => (a -> m b) -> a -> m (Record (s :-> b : s' :-> a : '[]))
traverseToFst f x =  (:*: x :*: RNil) <$> f x

traverseToSnd :: Applicative m => (a -> m b) -> a -> m (Record (s :-> a : s' :-> b : '[]))
traverseToSnd f x =  (\y -> x :*: y :*: RNil) <$> f x

fanout :: (x -> a) -> (x -> b) -> x -> Record (s :-> a : s' :-> b : '[])
fanout f g x = f x :*: g x :*: RNil

fanoutM :: Applicative m => (x -> m a) -> (x -> m b) -> x -> m (Record (s :-> a : s' :-> b : '[]))
fanoutM f g x = do
  y <- f x
  z <- g x
  return $ y :*: z :*: RNil

newtype RSource r m a = RSource { runSource :: Record r -> m a }

instance Functor m => IsoHKD (RSource r m) (s :-> a) where
  type HKD (RSource r m) (s :-> a) = Record r -> m a
  unHKD f = RSource $ fmap Val . f
  toHKD (RSource f) = fmap getVal . f

type XStep m a b = XRec (RSource a m) b

runSourceI :: Functor m => RSource r m a -> Record r -> m (Identity a)
runSourceI x = fmap Identity . runSource x

runXStep :: (IsoXRec (RSource a m) b, Applicative m) => XStep m a b -> Record a -> m (Record b)
runXStep x y = rtraverse (flip runSourceI y) (fromXRec x)

prependXStep :: (IsoXRec (RSource a m) b, Applicative m) => XStep m a b -> Record a -> m (Record (b ++ a))
prependXStep f x = fmap (<+> x) $ runXStep f x
