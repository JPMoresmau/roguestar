{-# OPTIONS_GHC -fglasgow-exts #-}

module DBPrivate
    (Reference(..),
     unsafeReference,
     toUID,
     Location(..),
     M,
     S,
     unsafeLocation,
     Position(..),
     Standing(..),
     Dropped(..),
     Inventory(..),
     Wielded(..),
     Constructed(..),
     TheUniverse(..),
     Subsequent(..),
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
-- DB and DBData import and re-export most of DBPrivate.  Other code should not
-- import DBPrivate since it could break the restrictions otherwise placed on
-- the type system.
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
-- The location of a Plane nested inside a building, such as a Stargate.
--
data Subsequent =
    Subsequent { subsequent_to :: PlaneRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- A relational data structure defining the location of any entity.
-- All of the type variables of Location are phantom types.
--
-- m represents the modification domain of the Location.  For example,
-- a Location M is the location of a moving entity.  The goal of m
-- is to ensure that the entity can not be changed when moving an entity,
-- e.g. Robert can not turn into Susan by walking across the street.
-- No function Location M e t -> Location M e t can be written that
-- changes the what entity the location references.
--
-- A Location S is the location of an still (unmoving) entity and may be
-- mutilated at will, but the type checker ensures that no such
-- mutilated Location may be used to move an entity.
--
-- Thus, we accept functions of the type 
-- (Location M e a -> Location M e b) -> DB (),
-- to move an object, a functions of the type
-- DB (Location S () ()) to get the location of an object.
--
-- e represents the type of entity, such as a Creature or Tool.
--
-- t represents the type of location, such as Standing or Dropped.
--
data M

data S

data Location m e t =
     IsStanding CreatureRef Standing 
   | IsDropped ToolRef Dropped
   | InInventory ToolRef Inventory
   | IsWielded ToolRef Wielded
   | IsConstructed BuildingRef Constructed
   | InTheUniverse PlaneRef
   | IsSubsequent PlaneRef Subsequent
    deriving (Read,Show,Eq,Ord)

unsafeLocation :: Location a b c -> Location d e f
unsafeLocation (IsStanding a b) = IsStanding a b
unsafeLocation (IsDropped a b) = IsDropped a b
unsafeLocation (InInventory a b) = InInventory a b
unsafeLocation (IsWielded a b) = IsWielded a b
unsafeLocation (IsConstructed a b) = IsConstructed a b
unsafeLocation (InTheUniverse a) = InTheUniverse a
unsafeLocation (IsSubsequent a b) = IsSubsequent a b

instance HierarchicalRelation (Location m e t) where
    parent (IsStanding _ t) = toUID $ standing_plane t
    parent (IsDropped _ t) = toUID $ dropped_plane t
    parent (InInventory _ t) = toUID $ inventory_creature t
    parent (IsWielded _ t) = toUID $ wielded_creature t
    parent (IsConstructed _ t) = toUID $ constructed_plane t
    parent (InTheUniverse _) = toUID UniverseRef
    parent (IsSubsequent _ t) = toUID $ subsequent_to t
    child (IsStanding e _) = toUID e
    child (IsDropped e _) = toUID e
    child (InInventory e _) = toUID e
    child (IsWielded e _) = toUID e
    child (IsConstructed e _) = toUID e
    child (InTheUniverse e) = toUID e
    child (IsSubsequent e _) = toUID e
