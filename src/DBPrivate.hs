{-# OPTIONS_GHC -fglasgow-exts #-}

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
     TheUniverse(..),
     CreatureRef,
     ToolRef,
     PlaneRef)
    where

import HierarchicalDatabase
import Facing
import CreatureData
import ToolData
import PlaneData

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
data TheUniverse = TheUniverse deriving (Read,Show)

type CreatureRef = Reference Creature
type ToolRef = Reference Tool
type PlaneRef = Reference Plane

-- |
-- A typesafe reference to any entity.
--
data Reference a = CreatureRef { uid:: Integer }
	         | PlaneRef { uid :: Integer }
	         | ToolRef { uid :: Integer }
                 | UniverseRef
		     deriving (Eq,Ord,Read,Show)

unsafeReference :: Reference a -> Reference b
unsafeReference (CreatureRef x) = CreatureRef x
unsafeReference (PlaneRef x) = PlaneRef x
unsafeReference (ToolRef x) = ToolRef x
unsafeReference UniverseRef = UniverseRef

toUID :: Reference a -> Integer
toUID (UniverseRef) = 0
toUID a = uid a

newtype Position = Position (Integer,Integer)
    deriving (Eq,Ord,Read,Show)

-- |
-- The location of a Creature standing on a Plane.
--
data Standing = 
    Standing { standing_plane :: PlaneRef,
               standing_position :: Position,
               standing_facing :: Facing }
    deriving (Read,Show)

-- |
-- The location of a Tool dropped on a Plane.
--
data Dropped = 
    Dropped { dropped_plane :: PlaneRef,
              dropped_position :: Position }
    deriving (Read,Show)

-- |
-- The location of a tool carried by a creature.
--
data Inventory =
    Inventory { inventory_creature :: CreatureRef }
    deriving (Read,Show)

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
   | InTheUniverse PlaneRef
    deriving (Read,Show)

unsafeLocation :: Location a b c -> Location d e f
unsafeLocation (IsStanding a b) = IsStanding a b
unsafeLocation (IsDropped a b) = IsDropped a b
unsafeLocation (InInventory a b) = InInventory a b
unsafeLocation (InTheUniverse a) = InTheUniverse a

instance HierarchicalRelation (Location m e t) where
    parent (IsStanding _ t) = toUID $ standing_plane t
    parent (IsDropped _ t) = toUID $ dropped_plane t
    parent (InInventory _ t) = toUID $ inventory_creature t
    parent (InTheUniverse _) = toUID UniverseRef
    child (IsStanding e _) = toUID e
    child (IsDropped e _) = toUID e
    child (InInventory e _) = toUID e
    child (InTheUniverse e) = toUID e
