{-# OPTIONS_GHC -fglasgow-exts #-}

module DBPrivate
    (Reference(..),
     unsafeReference,
     toUID,
     Location(..),
     unsafeLocation,
     Position(..),
     Standing(..),
     Dropped(..),
     Inventory(..),
     Wielded(..),
     Constructed(..),
     TheUniverse(..),
     Subsequent(..),
     Beneath(..),
     CreatureRef,
     ToolRef,
     PlaneRef,
     BuildingRef)
    where

import HierarchicalDatabase
import Facing
import CreatureData
import ToolData
import PlaneData
import BuildingData
import Position

--
-- For References and Locations we make considerable use of phantom types
-- to guarantee that such data structures are always consistent with the game logic,
-- e.g. a planet can not be wielded as a weapon.
--
-- DB and DBData import and re-export most of DBPrivate.  Other modules should not
-- import DBPrivate.
--

-- |
-- Type representing the entire universe.
--
data TheUniverse = TheUniverse deriving (Read,Show,Eq,Ord)

type CreatureRef = Reference Creature
type ToolRef = Reference Tool
type PlaneRef = Reference Plane
type BuildingRef = Reference Building

-- |
-- A typesafe reference to any entity.
--
data Reference a = CreatureRef { uid:: Integer }
	         | PlaneRef { uid :: Integer }
	         | ToolRef { uid :: Integer }
                 | BuildingRef { uid :: Integer }
                 | UniverseRef
		     deriving (Eq,Ord,Read,Show)

unsafeReference :: Reference a -> Reference b
unsafeReference (CreatureRef x) = CreatureRef x
unsafeReference (PlaneRef x) = PlaneRef x
unsafeReference (ToolRef x) = ToolRef x
unsafeReference (BuildingRef x) = BuildingRef x
unsafeReference UniverseRef = UniverseRef

toUID :: Reference a -> Integer
toUID (UniverseRef) = 0
toUID a = uid a

-- |
-- The location of a Creature standing on a Plane.
--
data Standing =
    Standing { standing_plane :: PlaneRef,
               standing_position :: Position,
               standing_facing :: Facing }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a Tool dropped on a Plane.
--
data Dropped =
    Dropped { dropped_plane :: PlaneRef,
              dropped_position :: Position }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a Building constructed on a Plane.
--
data Constructed =
    Constructed { constructed_plane :: PlaneRef,
                  constructed_position :: Position,
                  constructed_type :: BuildingType }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a tool carried by a creature.
--
data Inventory =
    Inventory { inventory_creature :: CreatureRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a weapon wielded in the hand of a creature.
--
data Wielded =
    Wielded { wielded_creature :: CreatureRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a Plane linked to from another Plane, such as with a Stargate.
--
data Subsequent =
    Subsequent { subsequent_to :: PlaneRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a dungeon plane.
--
data Beneath =
    Beneath { beneath_of :: PlaneRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- A relational data structure defining the location of any entity.
--
-- c represents the type of the child entity, such as a Creature or Tool.
--
-- p represents the type of the parent location, such as Standing or Dropped.
--
data Location e t =
     IsStanding CreatureRef Standing
   | IsDropped ToolRef Dropped
   | InInventory ToolRef Inventory
   | IsWielded ToolRef Wielded
   | IsConstructed BuildingRef Constructed
   | InTheUniverse PlaneRef
   | IsSubsequent PlaneRef Subsequent
   | IsBeneath PlaneRef Beneath
    deriving (Read,Show,Eq)

unsafeLocation :: Location a b -> Location c d
unsafeLocation (IsStanding a b) = IsStanding a b
unsafeLocation (IsDropped a b) = IsDropped a b
unsafeLocation (InInventory a b) = InInventory a b
unsafeLocation (IsWielded a b) = IsWielded a b
unsafeLocation (IsConstructed a b) = IsConstructed a b
unsafeLocation (InTheUniverse a) = InTheUniverse a
unsafeLocation (IsSubsequent a b) = IsSubsequent a b
unsafeLocation (IsBeneath a b) = IsBeneath a b

instance HierarchicalRelation (Location e t) where
    parent (IsStanding _ t) = toUID $ standing_plane t
    parent (IsDropped _ t) = toUID $ dropped_plane t
    parent (InInventory _ t) = toUID $ inventory_creature t
    parent (IsWielded _ t) = toUID $ wielded_creature t
    parent (IsConstructed _ t) = toUID $ constructed_plane t
    parent (InTheUniverse _) = toUID UniverseRef
    parent (IsSubsequent _ t) = toUID $ subsequent_to t
    parent (IsBeneath _ t) = toUID $ beneath_of t
    child (IsStanding e _) = toUID e
    child (IsDropped e _) = toUID e
    child (InInventory e _) = toUID e
    child (IsWielded e _) = toUID e
    child (IsConstructed e _) = toUID e
    child (InTheUniverse e) = toUID e
    child (IsSubsequent e _) = toUID e
    child (IsBeneath e _) = toUID e

