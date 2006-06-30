module HopList
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
hopListFactor = 16

toList :: HopList a -> [a]
toList (HopElem xs) = xs
toList (HopStack up right) = toList right
toList (HopNode down right) = toList down

fromList :: [a] -> HopList a
fromList xs = HopStack { hop_up=fromList_up (HopElem xs), hop_right=HopElem xs }

fromList_up :: HopList a -> HopList a
fromList_up param@(HopElem xs) = HopNode { hop_down=param, hop_right=fromList_up (HopElem (genericDrop hopListFactor xs)) }
fromList_up param@(HopNode down right) = HopNode { hop_down=param, hop_right=fromList_up (head $ genericDrop hopListFactor $ hop_rights param) }
fromList_up (HopStack up _) = up

hop_rights :: HopList a -> [HopList a]
hop_rights param = iterate hop_right param

index :: Integer -> HopList a -> a
index = index_ 1

index_ :: Integer -> Integer -> HopList a -> a
index_ 1 i (HopElem xs) = xs `genericIndex` i
index_ _ _ (HopElem _) = error "Depth of a HopElem is always 1"
index_ depth i param@(HopNode _ _) = index_ (depth `div` hopListFactor) (i `mod` depth)  $ 
				     hop_down $ head $ genericDrop (i `div` depth) $ hop_rights param
index_ depth i param@(HopStack _ _) = let next_depth = depth * hopListFactor
					  in if next_depth < i
					     then index_ next_depth i $ hop_up param
					     else index_ depth i $ hop_right param