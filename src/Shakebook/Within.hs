module Shakebook.Within (
  Within
, fromWithin
, toWithin
, within
, asWithin
, whatLiesWithin
, mapWithin
, mapWithinThrow
, moveWithin
) where

import Path
import RIO

newtype Within a t = Within (Path a Dir, Path Rel t)
  deriving (Typeable, Generic)

fromWithin :: Within a t -> Path a t
fromWithin (Within (x,y)) = x </> y

toWithin :: Path a Dir -> Path Rel t -> Within a t
toWithin = flip within

within :: Path Rel t -> Path a Dir -> Within a t
within y x = Within (x,y)

asWithin :: MonadThrow m => Path a t -> Path a Dir -> m (Within a t)
asWithin x y = stripProperPrefix y x >>= \z -> return (Within (y, z))

whatLiesWithin :: Within a t -> Path Rel t
whatLiesWithin (Within (x,y)) = y

mapWithin :: (Path Rel s -> Path Rel t) -> Within a s -> Within a t
mapWithin f (Within (x,y)) = Within (x, f y)

mapWithinThrow :: MonadThrow m => (Path Rel s -> m (Path Rel t)) -> Within a s -> m (Within a t)
mapWithinThrow f (Within (x,y)) = f y >>= \z -> return (Within (x, z))

moveWithin :: (Path a Dir -> Path b Dir) -> Within a t -> Within b t
moveWithin f (Within (x,y)) = Within ((f x), y)

moveWithinThrow :: MonadThrow m => (Path a Dir -> m (Path b Dir)) -> Within a t -> m (Within b t)
moveWithinThrow f (Within (x,y)) = f x >>= \z -> return (Within (z,y))
