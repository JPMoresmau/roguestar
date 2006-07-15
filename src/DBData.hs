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
     DBLocation(..))
    where

data DBReference = DBCreatureRef CreatureRef
		 | DBPlaneRef PlaneRef
		 deriving (Eq,Ord,Read,Show)

data DBLocation = DBCoordinateLocation (Integer,Integer)
		deriving (Read,Show)

newtype CreatureRef = CreatureRef Integer deriving (Eq,Ord,Read,Show)
newtype PlaneRef = PlaneRef Integer deriving (Eq,Ord,Read,Show)

class DBRef a where
    toDBReference :: (DBRef a) => a -> DBReference

instance DBRef CreatureRef where
    toDBReference x = DBCreatureRef x

instance DBRef PlaneRef where
    toDBReference x = DBPlaneRef x

