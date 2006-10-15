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

module DBData
    (CreatureRef(..),
     PlaneRef(..),
     DBRef(..),
     DBReference(..),
     DBLocation(..),
     toCoordinateLocation,
     toCoordinateFacingLocation,
     isCreatureRef,
     isPlaneRef,
     toCreatureRef,
     toPlaneRef)
    where

import Facing

data DBReference = DBCreatureRef CreatureRef
		 | DBPlaneRef PlaneRef
		 deriving (Eq,Ord,Read,Show)

isCreatureRef :: DBReference -> Bool
isCreatureRef (DBCreatureRef {}) = True
isCreatureRef _ = False

isPlaneRef :: DBReference -> Bool
isPlaneRef (DBCreatureRef {}) = True
isPlaneRef _ = False

toCreatureRef :: (DBRef a) => a -> CreatureRef
toCreatureRef x = case toDBReference x of
				       DBCreatureRef creature_ref -> creature_ref
				       _ -> error "not a DBCreatureRef"

toPlaneRef :: (DBRef a) => a -> PlaneRef
toPlaneRef x = case toDBReference x of
				    DBPlaneRef plane_ref -> plane_ref
				    _ -> error "not a DBPlaneRef"

data DBLocation = DBCoordinateLocation (Integer,Integer)
                | DBCoordinateFacingLocation ((Integer,Integer),Facing)
		deriving (Read,Show)

-- |
-- Converts a DBLocation to a location in (x,y) form, or nothing if there is
-- no valid (x,y) interpretation of the DBLocation.
--
toCoordinateLocation :: DBLocation -> Maybe (Integer,Integer)
toCoordinateLocation (DBCoordinateLocation xy) = Just xy
toCoordinateLocation (DBCoordinateFacingLocation (xy,_)) = Just xy

-- |
-- Converts a DBLocation to a location in ((x,y),facing) form, or nothing
-- if there is no such valid interpretation of the DBLocation.  DBLocations
-- that contain only (x,y) coordinates will return with a facing of Here.
--
toCoordinateFacingLocation :: DBLocation -> Maybe ((Integer,Integer),Facing)
toCoordinateFacingLocation (DBCoordinateLocation xy) = Just (xy,Here)
toCoordinateFacingLocation (DBCoordinateFacingLocation xyf) = Just xyf

newtype CreatureRef = CreatureRef Integer deriving (Eq,Ord,Read,Show)
newtype PlaneRef = PlaneRef Integer deriving (Eq,Ord,Read,Show)

class DBRef a where
    toDBReference :: a -> DBReference
    toUID :: a -> Integer

instance DBRef DBReference where
    toDBReference x = x
    toUID (DBPlaneRef ref) = toUID ref
    toUID (DBCreatureRef ref) = toUID ref

instance DBRef CreatureRef where
    toDBReference x = DBCreatureRef x
    toUID (CreatureRef x) = x

instance DBRef PlaneRef where
    toDBReference x = DBPlaneRef x
    toUID (PlaneRef x) = x
