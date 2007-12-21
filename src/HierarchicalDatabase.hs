--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
--  Copyright (C) 2006,2007 Christopher Lane Hinson <lane@downstairspeople.org>  
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

module HierarchicalDatabase
    (HierarchicalDatabase,
     HierarchicalRelation(..),
     HierarchicalDatabase.empty,
     HierarchicalDatabase.insert,
     HierarchicalDatabase.delete,
     HierarchicalDatabase.lookup,
     lookupChildren,
     lookupParent,
     parentOf,
     childrenOf,
     HierarchicalDatabase.toList,
     HierarchicalDatabase.fromList,
     insidenessTests)
    where

import Data.Map as Map
import Data.List as List
import Tests
import Data.Maybe as Maybe

class HierarchicalRelation a where
    parent :: a -> Integer
    child :: a -> Integer

instance (Integral a,Integral b) => HierarchicalRelation (a,b) where
    parent = toInteger . snd
    child = toInteger . fst

data HierarchicalDatabase a = 
    HierarchicalDatabase {
        hd_children :: (Map Integer [Integer]),
        hd_parent :: (Map Integer a) }

instance (Show a) => Show (HierarchicalDatabase a) where
    show imap = show $ HierarchicalDatabase.toList imap

instance (HierarchicalRelation a,Read a) => Read (HierarchicalDatabase a) where
    readsPrec n = \v -> Prelude.map (\(x,y) -> (HierarchicalDatabase.fromList x,y)) (readsPrec n v)

empty :: HierarchicalDatabase a
empty = HierarchicalDatabase (Map.empty) (Map.empty)

-- |
-- O(log n)  Inserts the specified (parent,child,user_data) pair into the
-- InsidessMap.  If the given child already has a parent, that parent is
-- replaced by the new one.
--
insert :: (HierarchicalRelation a) => a -> HierarchicalDatabase a -> HierarchicalDatabase a
insert a the_map =
    HierarchicalDatabase {
        hd_children = alter (Just . maybe [child a] (child a :)) (parent a) $ 
                          hd_children $ HierarchicalDatabase.delete (child a) the_map,
        hd_parent = Map.insert (child a) a $ hd_parent the_map }

-- |
-- Deletes the specified object from this insideness map.
--
delete :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> HierarchicalDatabase a
delete x the_map =
    HierarchicalDatabase {
        hd_children = maybe (hd_children the_map) (\p -> update deleteChildFromList p $ hd_children the_map) xsParent,
        hd_parent = Map.delete x $ hd_parent the_map }
    where deleteChildFromList l = case List.delete x l of
                                       [] -> Nothing
                                       l' -> Just l'
          xsParent = parentOf x the_map

-- |
-- Answers the parent of an element, or nothing if the element
-- is not listed as a child in this HierarchicalDatabase.
--
parentOf :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> Maybe Integer
parentOf x the_map = fmap parent $ Map.lookup x $ hd_parent the_map

-- |
-- Answers the parent relation and all children relations for a given key.
--
lookup :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> (Maybe a,[a])
lookup x the_map = (Map.lookup x $ hd_parent the_map,
                    maybe [] (Maybe.mapMaybe (flip Map.lookup (hd_parent the_map))) $ Map.lookup x $ hd_children the_map)

lookupChildren :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> [a]
lookupChildren x the_map = snd $ HierarchicalDatabase.lookup x the_map

lookupParent :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> Maybe a
lookupParent x the_map = fst $ HierarchicalDatabase.lookup x the_map

-- |
-- Answers a list of the children of an element, or the null list if the element is
-- not listed as a parent in this HierarchicalDatabase.
--
childrenOf :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> [Integer]
childrenOf x the_map = maybe [] id $ Map.lookup x (hd_children the_map)
	
	
-- |
-- Converts a HierarchicalDatabase into a list of relations.
--
toList :: HierarchicalDatabase a -> [a]
toList the_map = List.map snd $ Map.toList $ hd_parent the_map

-- |
-- Converts a list of relations into a HierarchicalDatabase.
--
fromList :: (HierarchicalRelation a) => [a] -> HierarchicalDatabase a
fromList as = foldr (HierarchicalDatabase.insert) HierarchicalDatabase.empty as

data ExampleRelation = ExampleRelation (Integer,Integer,Bool)

instance HierarchicalRelation ExampleRelation where
    parent (ExampleRelation (n,_,_)) = n
    child (ExampleRelation (_,n,_)) = n

example1 :: HierarchicalDatabase ExampleRelation
example1 = HierarchicalDatabase.fromList $ List.map ExampleRelation 
                                               [(1,13,True),
						(1,(-5),True),
						(1,1,True),
						(1,7,True),
						(1,15,True),
						(2,0,False),
						(3,12,True),
						(3,9,False),
						(3,(-3),True),
						(4,100,False),
						(4,(-6),False),
						(4,14,False)]

testParent :: TestCase
testParent = if (parentOf 0 example1) == (Just 2)
	     then return (Passed "testParent")
	     else return (Failed "testParent")

testChildren :: TestCase
testChildren = if (length $ childrenOf 1 example1) == 5
	       then return (Passed "testChildren")
	       else return (Failed "testChildren")

testUserData :: TestCase
testUserData = let child_records = lookupChildren 1 example1
		   in if (all (\(ExampleRelation (_,_,b)) -> b) child_records)
		      then return (Passed "testUserDatas")
		      else return (Failed "testUserDatas")

testChildrenCorrect :: TestCase
testChildrenCorrect = let the_children = childrenOf 4 example1
			  in if (all even the_children)
			     then return (Passed "testChildrenCorrect")
			     else return (Failed "testChildrenCorrect")

testDelete :: TestCase
testDelete = let deleted = HierarchicalDatabase.delete 0 $ HierarchicalDatabase.delete (-6) $ example1
		 in if ((length $ childrenOf 4 deleted) == 2 &&
			(isNothing $ parentOf 0 deleted))
		 then return (Passed "testDelete")
		 else return (Failed "testDelete")

insidenessTests :: [TestCase]
insidenessTests = [testParent,testChildren,testUserData,testChildrenCorrect,testDelete]