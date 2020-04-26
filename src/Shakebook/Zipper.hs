{-| Zipper utils that weren't in Control.Comonad.Store.Zipper -}

module Shakebook.Zipper (
  paginate
, zipperNextMaybe
, zipperPreviousMaybe
, zipperWithin
) where

import Control.Comonad.Store
import Control.Comonad.Store.Zipper
import Data.List.Split
import RIO

-- Turn a list into a zipper of chunks of length n
paginate :: Int -> [a] -> Maybe (Zipper [] [a])
paginate n = zipper . chunksOf n

-- Return the peek of the next element if it exists.
zipperNextMaybe :: Zipper [] a -> Maybe a
zipperNextMaybe xs = if pos xs < size xs-1 then Just (peeks (+1) xs) else Nothing

-- Return the peek of the previous element if it exists.
zipperPreviousMaybe :: Zipper [] a -> Maybe a
zipperPreviousMaybe xs = if pos xs > 0 then Just (peeks (+ (-1)) xs) else Nothing

-- Return a list of elements within 'r' hops either side of the zipper target.
zipperWithin :: Int -> Zipper [] a -> [a]
zipperWithin r xs = (`peek` xs) <$>  [(max 0 (pos xs - r)) .. (min (size xs -1) (pos xs + r))]
