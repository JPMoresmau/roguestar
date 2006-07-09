--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module HopList
    (HopList,
     toList,
     fromList,
     hopTail,
     index,
     hopLookup,
     hopListTests)
    where

import Data.List as List
import Tests

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
data HopList a = HopStack { hop_up :: HopList a, hop_right :: HopList a }
	       | HopNode { hop_down :: !(HopList a), hop_right :: HopList a }
	       | HopElem [a]

hopListFactor :: Integer
hopListFactor = 16

toList :: HopList a -> [a]
toList (HopElem xs) = xs
toList (HopStack _ right) = toList right
toList (HopNode down _) = toList down

fromList :: [a] -> HopList a
fromList xs = HopStack { hop_up=fromList_up (HopElem xs), hop_right=HopElem xs }

fromList_up :: HopList a -> HopList a
fromList_up param@(HopElem xs) = seq param $ HopNode { hop_down=param, hop_right=fromList_up (HopElem (genericDrop hopListFactor xs)) }
fromList_up param@(HopNode _ _) = seq param $ HopNode { hop_down=param, hop_right=fromList_up ((hop_rights param) `genericIndex` hopListFactor) }
fromList_up param@(HopStack _ _) = hop_up param

hop_rights :: HopList a -> [HopList a]
hop_rights param = iterate hop_right param

-- |
-- Answers the rest of a HopList starting from the specified index.
-- ((fromList xs) `hopTail` 5) is equivalent to (drop 5 xs).
--
hopTail :: HopList a -> Integer -> [a]
hopTail hl i = hopTail_ hl 1 i

hopTail_ :: HopList a -> Integer -> Integer -> [a]
hopTail_ (HopElem xs) 1 i = genericDrop i xs
hopTail_ (HopElem _) _ _ = error "Depth of a HopElem is always 1"
hopTail_ param@(HopNode _ _) depth i = hopTail_ 
                                       (hop_down $ head $ genericDrop (i `div` depth) $ hop_rights param)
                                       (depth `div` hopListFactor) 
                                       (i `mod` depth)
hopTail_ param@(HopStack _ _) depth i = let next_depth = depth * hopListFactor
					    in if next_depth < i
					       then hopTail_ (hop_up param) next_depth i
					       else hopTail_ (hop_right param) depth i

-- |
-- Answers the element at the specified index.  ((fromList xs) `index` 5)
-- is equivalent to (xs !! 5).
--
index :: HopList a -> Integer -> a
index hl i = head $ hopTail hl i

-- |
-- As index, but returns in a monad if the element is available
-- or fails if it is beyond the end of the list.
--
hopLookup :: Monad m => HopList a -> Integer -> m a
hopLookup hl i = case (hopTail hl i) of
				  [] -> fail ("no element at index " ++ (show i))
				  xs -> return $ head xs

exampleHopListInfinite :: HopList Int
exampleHopListInfinite = fromList [0,2..]

exampleHopListFinite :: HopList Int
exampleHopListFinite = fromList [0,2..2000]

hopListTests :: [TestCase]
hopListTests = [hopListTestZeroIndex,hopListTestSmallIndex,hopListTestLargeIndex,hopListTestOutOfBoundsIndex]

hopListTestZeroIndex :: TestCase
hopListTestZeroIndex = test "hopListTestZeroIndex" 
		       ((exampleHopListInfinite `hopLookup` 0) == Just 0)

hopListTestSmallIndex :: TestCase
hopListTestSmallIndex = test "hopListTestSmallIndex"
			((exampleHopListInfinite `hopLookup` 5) == Just 10)

hopListTestLargeIndex :: TestCase
hopListTestLargeIndex = test "hopListTestLargeIndex"
			((exampleHopListInfinite `hopLookup` 500000) == Just 1000000)

hopListTestOutOfBoundsIndex :: TestCase
hopListTestOutOfBoundsIndex = test "hopListTestOutOfBoundsIndex"
			      ((exampleHopListFinite `hopLookup` 500000) == Nothing)