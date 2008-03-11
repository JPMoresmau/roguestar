
module SegHopList
    (SegHopList,SegHopList.fromList,SegHopList.index)
    where

import SegmentList
import HopList
import Data.Array

-- |
-- A system that combines the benefits of the SegmentList and the HopList
-- to access data arbitrarily far away in an infinite list quickly.
--
type SegHopList a = HopList (Array Int a)

fromList :: [a] -> SegHopList a
fromList xs = HopList.fromList (segmentList xs)

index :: SegHopList a -> Integer -> a
index shl i = (shl `HopList.index` (i `div` segmentSizeI)) ! ((fromInteger i) `mod` segmentSizei)
