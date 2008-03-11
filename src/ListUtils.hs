
module ListUtils 
    (listByFrequency,
     count,
     bidirect,
     bidirectionalAccessor1D,
     bidirectionalAccessor2D,
     monodirect,
     monodirectionalList1D,
     monodirectionalList2D,
     cachedAccessor1D,
     cachedAccessor2D)
    where

import Data.List
import SegHopList

-- |
-- Converts a list of elements to an infinite list of those same elements such
-- that the frequency of an element of the result is related to how early
-- that element occurs in the parameter.  Each subsequent element in the parameter
-- occurs half as often (and first occurs twice as late) as the one before.
-- [a,b,c,d] becomes (cycle [a,b,a,c,a,b,a,d])
--
listByFrequency :: [a] -> [a]
listByFrequency (x:[]) = repeat x
listByFrequency (x:xs) = x : (intersperse x $ listByFrequency xs)
listByFrequency [] = error "Can't do anything with an empty list."

-- |
-- count 1 [2,5,1,4,1,1] is 3, because 1 occurs three times.
--
count :: Eq a => a -> [a] -> Integer
count element lst = genericLength $ elemIndices element lst

-- |
-- Maps integers in the range [-inf .. inf] to [0 .. inf]
--
bidirect :: Integer -> Integer
bidirect n = if n >= 0
	     then (2*n)
	     else (2*(-n)-1)

-- |
-- Inverse operation of bidirect.
--
monodirect :: Integer -> Integer
monodirect n = if (even n)
	       then n `div` 2
	       else -(n `div` 2)

-- |
-- Accessor to reference a one-dimensional list as a bidirectional list.
-- In other words, the indexes becomes:
-- [0,-1,1,-2,2,-3,3,-4,4,-5,5 ...]
--
bidirectionalAccessor1D :: [a] -> (Integer -> a)
bidirectionalAccessor1D xs = let sh_list = SegHopList.fromList xs
				 in (\i -> sh_list `SegHopList.index` (bidirect i))

-- |
-- Accessor to reference a two-dimensional list as a bidirectional two-dimensional list.
-- The outer list is considered to be the y-axis, and the inner list the x-axis, if 
-- elements are references by (x,y)
--
bidirectionalAccessor2D :: [[a]] -> ((Integer,Integer) -> a)
bidirectionalAccessor2D xss = let sh_lists = SegHopList.fromList $ map SegHopList.fromList xss
				  in (\(x,y) -> (sh_lists `SegHopList.index` (bidirect y)) `SegHopList.index` (bidirect x))

-- |
-- Inverse operation of bidirectionalAccessor1D
--
monodirectionalList1D :: (Integer -> a) -> [a]
monodirectionalList1D fn = map (fn . monodirect) [0..]

-- |
-- Inverse operation of bidirectionalAccessor2D
--
monodirectionalList2D :: ((Integer,Integer) -> a) -> [[a]]
monodirectionalList2D fn = let zero_dot_dot = [0..]
			       pairs = [[(monodirect x,monodirect y) | x <- zero_dot_dot] | y <- zero_dot_dot]
			       in map (map fn) pairs

-- |
-- Combines monodirectionalList1D and bidirectionalAccessor1D to create a cached version
-- of the original function.  If the original was a sufficiently expensive function for which
-- the same value is queried many times, then the cached version may be faster, at the expense
-- of memory.
cachedAccessor1D :: (Integer -> a) -> (Integer -> a)
cachedAccessor1D = bidirectionalAccessor1D . monodirectionalList1D

-- |
-- 2D version of cachedAccessor1D.
--
cachedAccessor2D :: ((Integer,Integer) -> a) -> ((Integer,Integer) -> a)
cachedAccessor2D = bidirectionalAccessor2D . monodirectionalList2D
