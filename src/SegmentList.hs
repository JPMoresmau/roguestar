
module SegmentList
    (segmentSizei,segmentSizeI,segmentList,segmentIndex)
    where

import Data.List
import Data.Array

segmentSizei :: Int
segmentSizei = 100

segmentSizeI :: Integer
segmentSizeI = toInteger segmentSizei

-- |
-- Constructs a list in which chunks of sequential elements are held together
-- in an array, to improve access time.  This is only intended for
-- use in an infinite list (otherwise just pack the entire thing
-- in one array).
-- 
segmentList :: [a] -> [Array Int a]
segmentList xs = let (firstGroup,restGroups) = seqSplitAt segmentSizei xs
		     in (listArray (0,segmentSizei-1) firstGroup) :
			    (segmentList restGroups)

seqSplitAt :: Int -> [a] -> ([a],[a])
seqSplitAt 0 xs = ([],xs)
seqSplitAt i (x:xs) = let rest = (seqSplitAt (i-1) xs)
			in seq x $ (x : (fst rest),snd rest)
seqSplitAt i [] = error ("Tried to access " ++ (show i) ++ "'th element of []")

-- |
-- Retrieve an element from a segment list by index.
--
segmentIndex :: [Array Int a] -> Integer -> a
segmentIndex xss i = (xss `genericIndex` (i `div` segmentSizeI)) ! ((fromInteger i) `mod` segmentSizei)
