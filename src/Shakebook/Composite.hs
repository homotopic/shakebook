{-# LANGUAGE UndecidableInstances #-}
module Shakebook.Composite where

import           Composite.Record
import           Development.Shake.Plus hiding ((:->))
import           RIO

instance Hashable a => Hashable (s :-> a) where
  hashWithSalt n x = hashWithSalt n $ getVal x

instance Binary a => Binary (s :-> a) where
  put = put . getVal
  get = fmap (runIdentity . val) get

instance NFData (s :-> a) where
  rnf x = seq x ()

instance Binary (Record '[])

instance (Binary a, Binary (Record xs), x ~ (s :-> a)) => Binary (Record (x : xs)) where
  put (x :*: xs) = put x >> put xs
  get = liftA2 (:*:) get get

instance NFData (Record '[]) where
  rnf RNil = ()

instance (NFData a, NFData (Record xs), x ~ (s :-> a)) => NFData (Record (x : xs)) where
  rnf (x :*: xs) = rnf x `seq` rnf xs

instance Hashable (Record '[]) where
  hashWithSalt n RNil = n `hashWithSalt` ()

instance (Hashable a, Hashable (Record xs), x ~ (s :-> a)) => Hashable (Record (x : xs)) where
  hashWithSalt n (x :*: xs) = n `hashWithSalt` x `hashWithSalt` xs
