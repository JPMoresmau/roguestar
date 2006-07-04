module HopList
    (main)
    where

import Data.List as List

-- |
-- A data structure that is almost, but not exactly, completely unlike a skip list.
-- Strictly speaking, skip lists are probabilistic data structures over sorted elements.
-- This HopList implementation just allows O( log n ) access to elements of a haskell list.
-- Like a skip list, it uses a stack of parallel arrays to quickly traverse a list.
-- It supports infinite lists.
--
-- The HopList looks something like this:
--
-- 00                              ->                              16
-- 00      ->      04      ->      08      ->      12      ->      16      ->      20
-- 00  01  02  03  04  05  06  07  08  09  10  11  12  13  14  15  16  17  18  19  20
--
-- The list representing the stack of HopLists is called the perpendicular list.  Each list 
-- that "hops" over the list below it in the stack is called a parallel list.
--
data HopList a = HopStack { hop_up :: HopList a, hop_right :: HopList a }
	       | HopNode { hop_down :: !(HopList a), hop_right :: HopList a }
	       | HopElem [a]

hopListFactor :: Integer
hopListFactor = 64

toList :: HopList a -> [a]
toList (HopElem xs) = xs
toList (HopStack up right) = toList right
toList (HopNode down right) = toList down

fromList :: [a] -> HopList a
fromList xs = HopStack { hop_up=fromList_up (HopElem xs), hop_right=HopElem xs }

fromList_up :: HopList a -> HopList a
fromList_up param@(HopElem xs) = seq param $ HopNode { hop_down=param, hop_right=fromList_up (HopElem (genericDrop hopListFactor xs)) }
fromList_up param@(HopNode down right) = seq param $ HopNode { hop_down=param, hop_right=fromList_up ((hop_rights param) `genericIndex` hopListFactor) }
fromList_up (HopStack up _) = up

hop_rights :: HopList a -> [HopList a]
hop_rights param = iterate hop_right param

index :: HopList a -> Integer -> a
index hl i = index_ hl 1 i

index_ :: HopList a -> Integer -> Integer -> a
index_ (HopElem xs) 1 i = xs `genericIndex` i
index_ (HopElem _) _ _ = error "Depth of a HopElem is always 1"
index_ param@(HopNode _ _) depth i = index_ 
                                     (hop_down $ head $ genericDrop (i `div` depth) $ hop_rights param)
                                     (depth `div` hopListFactor) 
                                     (i `mod` depth)
index_ param@(HopStack _ _) depth i = let next_depth = depth * hopListFactor
					  in if next_depth < i
					     then index_ (hop_up param) next_depth i
					     else index_ (hop_right param) depth i

exampleHopList :: HopList Int
exampleHopList = fromList [0,2..]
