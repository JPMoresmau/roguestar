module InsidenessMap
    (InsidenessMap.empty,
     InsidenessMap.insert,
     InsidenessMap.delete,
     parent,
     children,
     InsidenessMap.toList,
     InsidenessMap.fromList,
     insidenessTests,
     testParent)
    where

import Data.Map as Map
import Data.List as List
import Tests
import Data.Maybe

data InsidenessMap a b c = 
    InsidenessMap
    (Map a [b])
    (Map b (a,c))

empty :: InsidenessMap a b c
empty = InsidenessMap (Map.empty) (Map.empty)

-- |
-- O(log n)  Inserts the specified (parent,child,user_data) pair into the
-- InsidessMap.  If the given child already has a parent, that parent is
-- replaced by the new one.
--
insert :: (Ord a, Ord b) => (a,b,c) -> InsidenessMap a b c -> InsidenessMap a b c
insert (a,b,c) the_map@(InsidenessMap _ bac) =
    let (InsidenessMap clean_ab _) = purgeAB b the_map
        in InsidenessMap
               (insertWith (++) a [b] clean_ab)
               (Map.insert b (a,c) bac)

-- |
-- Deletes the specified child from this insideness map.
--
delete :: (Ord a, Ord b) => b -> InsidenessMap a b c -> InsidenessMap a b c
delete b the_map@(InsidenessMap _ bac) =
    let (InsidenessMap clean_ab _) = purgeAB b the_map 
	in InsidenessMap
	   (clean_ab)
	   (Map.delete b bac)

-- |
-- Given a child, b, removes the entry for b from the ab map of
-- (InsidenessMap ab bac).  If b exists in the InsidenessMap, then
-- the result is a corrupt InsidenessMap, because a mapping exists
-- from b to (a,b,c) in bac, but no mapping exists from a to b in
-- ab.  This is a helper function for several other functions in
-- this module.
--
purgeAB :: (Ord a, Ord b) => b -> InsidenessMap a b c -> InsidenessMap a b c
purgeAB b (InsidenessMap ab bac) =
    let old_parent = Map.lookup b bac
	clean_ab = maybe ab (\x -> adjust (List.delete b) (fst x) ab) old_parent
	in InsidenessMap clean_ab bac

-- |
-- Answers the parent of an element, or nothing if the element
-- is not listed in this InsidenessMap.
--
parent :: (Ord a, Ord b) => b -> InsidenessMap a b c -> Maybe a
parent b (InsidenessMap _ bac) = maybe Nothing (Just . fst) (Map.lookup b bac)

-- |
-- Answers a list of the children of an element, in the form
-- (parent,child,user_data).  "parent" in the result will always
-- be the first parameter.
--
children :: (Ord a, Ord b) => a -> InsidenessMap a b c -> [(a,b,c)]
children the_parent (InsidenessMap ab bac) =
    let maybe_the_children = Map.lookup the_parent ab
        in maybe
	       []
               (\the_children -> zip3 
		(repeat the_parent)
		(the_children)
		(List.map (\x -> snd $ (bac ! x)) the_children))
	       maybe_the_children
	
	
-- |
-- Converts an InsidenessMap into a list of the form [(parent,child,user_data)]
--
toList :: (Ord a, Ord b) => InsidenessMap a b c -> [(a,b,c)]
toList the_map@(InsidenessMap ab _) = foldr1 (++) (List.map 
						   (\the_parent -> children the_parent the_map) 
						   (keys ab))

-- |
-- Converts a list of the form [(parent,child,user_data)] into
-- an InsidenessMap
--
fromList :: (Ord a, Ord b) => [(a,b,c)] -> InsidenessMap a b c
fromList the_list = foldr InsidenessMap.insert InsidenessMap.empty the_list

exampleInsidenessMap1 :: InsidenessMap String Integer Bool
exampleInsidenessMap1 = InsidenessMap.fromList [("LotsOfTrueIntegers",13,True),
						("LotsOfTrueIntegers",(-5),True),
						("LotsOfTrueIntegers",1,True),
						("LotsOfTrueIntegers",7,True),
						("LotsOfTrueIntegers",15,True),
						("OneFalseInteger",0,False),
						("AFewDifferentIntegers",12,True),
						("AFewDifferentIntegers",9,False),
						("AFewDifferentIntegers",(-3),True),
						("SomeEvenIntegers",100,False),
						("SomeEvenIntegers",(-6),False),
						("SomeEvenIntegers",14,False)]

testParent :: TestCase
testParent = if (parent 0 exampleInsidenessMap1) == (Just "OneFalseInteger")
	     then return (Passed "testParent")
	     else return (Failed "testParent")

testChildren :: TestCase
testChildren = if (length $ children "LotsOfTrueIntegers" exampleInsidenessMap1) == 5
	       then return (Passed "testChildren")
	       else return (Failed "testChildren")

testUserData :: TestCase
testUserData = let (_,_,user_datas) = unzip3 $ children "LotsOfTrueIntegers" exampleInsidenessMap1
		   in if (and user_datas)
		      then return (Passed "testUserDatas")
		      else return (Failed "testUserDatas")

testChildrenCorrect :: TestCase
testChildrenCorrect = let (_,the_children,_) = unzip3 $ children "SomeEvenIntegers" exampleInsidenessMap1
			  in if (all even the_children)
			     then return (Passed "testChildrenCorrect")
			     else return (Failed "testChildrenCorrect")

testDelete :: TestCase
testDelete = let deleted = InsidenessMap.delete 0 $ InsidenessMap.delete (-6) $ exampleInsidenessMap1
		 in if ((length $ children "SomeEvenIntegers" deleted) == 2 &&
			(isNothing $ parent 0 deleted))
		 then return (Passed "testDelete")
		 else return (Failed "testDelete")

insidenessTests :: [TestCase]
insidenessTests = [testParent,testChildren,testUserData,testChildrenCorrect,testDelete]