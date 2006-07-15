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

module DB
    (DB,
     initial_db,
     DB_BaseType,
     dbAddCreature,
     dbAddPlane,
     dbGetCreature,
     dbGetPlane,
     dbNextRandomInteger,
     dbNextRandomIntegerStream)
    where

import DBData
import CreatureData
import PlaneData
import Control.Monad.State
import System.Time
import RNG
import Data.Map as Map
import InsidenessMap

-- |
-- Random access form of the roguestar database.
--
data DB_BaseType = DB_BaseType { random_number_stream_stream :: [[Integer]],
				 next_object_ref :: Integer,
			         db_creatures :: Map CreatureRef Creature,
				 db_planes :: Map PlaneRef Plane,
				 db_inside :: InsidenessMap DBReference DBReference DBLocation}

-- |
-- Serial form of the roguestar database.
--
data DB_Persistant_BaseType = DB_Persistant_BaseType { random_number_generator_seed_ :: Integer,
                                                       next_object_ref_ :: Integer,
						       db_creatures_ :: [(CreatureRef,Creature)],
						       db_planes_ :: [(PlaneRef,Plane)],
						       db_inside_ :: [(DBReference,DBReference,DBLocation)]}
                              deriving (Read,Show)

toPersistant :: DB_BaseType -> DB_Persistant_BaseType
toPersistant db = DB_Persistant_BaseType {
					  random_number_generator_seed_ = (random_number_stream_stream db) !! 0 !! 0,
					  next_object_ref_ = next_object_ref db,
					  db_creatures_ = Map.toList $ db_creatures db,
					  db_planes_ = Map.toList $ db_planes db,
					  db_inside_ = InsidenessMap.toList $ db_inside db
					 }

fromPersistant :: DB_Persistant_BaseType -> DB_BaseType
fromPersistant persistant = DB_BaseType {
					 random_number_stream_stream = randomIntegerStreamStream $ random_number_generator_seed_ persistant,
					 next_object_ref = next_object_ref_ persistant,
					 db_creatures = Map.fromList $ db_creatures_ persistant,
					 db_planes = Map.fromList $ db_planes_ persistant,
					 db_inside = InsidenessMap.fromList $ db_inside_ persistant
					}

fromPersistant_tupled :: (DB_Persistant_BaseType,String) -> (DB_BaseType,String)
fromPersistant_tupled (persistant,str) = (fromPersistant persistant,str)

instance Read DB_BaseType where
    readsPrec n = \x -> Prelude.map fromPersistant_tupled (readsPrec n x)

instance Show DB_BaseType where
    show db = show (toPersistant db)

type DB a = State DB_BaseType a

-- |
-- Generates an initial DB state.
--
initial_db :: IO DB_BaseType
initial_db = do (TOD seconds picos) <- getClockTime
		return DB_BaseType { random_number_stream_stream = randomIntegerStreamStream (seconds + picos),
				     next_object_ref = 0,
				     db_creatures = Map.fromList [],
				     db_planes = Map.fromList [],
				     db_inside = InsidenessMap.fromList []
				   }

-- |
-- Gets the next ObjectRef integer, after incrementing it.
--
dbNextObjectRef :: DB Integer
dbNextObjectRef = do db <- get
		     let result = next_object_ref db
			 in do put (db { next_object_ref=(succ result) })
			       return result

-- |
-- Adds something to a map in the database using a new object reference.
--
dbAddObjectComposable :: (Integer -> a,(a,b) -> DB c) -> b -> DB a
dbAddObjectComposable (ref_ctor,update_fn) thing = do ref <- dbNextObjectRef
						      update_fn (ref_ctor ref,thing)
						      return $ ref_ctor ref

-- |
-- Adds a new Creature to the database.
--
dbAddCreature :: Creature -> DB CreatureRef
dbAddCreature = dbAddObjectComposable (CreatureRef,dbPutCreature)

-- |
-- Adds a new Plane to the database.
--
dbAddPlane :: Plane -> DB PlaneRef
dbAddPlane = dbAddObjectComposable (PlaneRef,dbPutPlane)

-- |
-- Puts an object into the database using getter and setter functions.
--
dbPutObjectComposable :: (Ord a) => (DB_BaseType -> Map a b,Map a b -> DB_BaseType -> DB_BaseType) -> (a,b) -> DB ()
dbPutObjectComposable (get_map_fn,put_map_fn) (key,thing) = do db <- get
							       put $ put_map_fn (Map.insert key thing $ get_map_fn db) db 

-- |
-- Puts a Creature under an arbitrary CreatureRef.
--
dbPutCreature :: (CreatureRef,Creature) -> DB ()
dbPutCreature = dbPutObjectComposable (db_creatures,\x db_base_type -> db_base_type { db_creatures = x })

-- |
-- Puts a Plane under an arbitrary PlaneRef
--
dbPutPlane :: (PlaneRef,Plane) -> DB ()
dbPutPlane = dbPutObjectComposable (db_planes,\x db_base_type -> db_base_type { db_planes = x })

-- |
-- Gets an object from the database using getter functions.
--
dbGetObjectComposable :: (Ord a) => (DB_BaseType -> Map a b) -> a -> DB b
dbGetObjectComposable get_fn ref = do db <- get
				      Map.lookup ref $ get_fn db

-- |
-- Gets a Creature from a CreatureRef
--
dbGetCreature :: CreatureRef -> DB Creature
dbGetCreature = dbGetObjectComposable db_creatures

-- |
-- Gets a Plane from a PlaneRef
--
dbGetPlane :: PlaneRef -> DB Plane
dbGetPlane = dbGetObjectComposable db_planes

-- |
-- Generates and returns the next random Integer.
--
dbNextRandomInteger :: DB Integer
dbNextRandomInteger = do db <- get
			 let rngss0 = random_number_stream_stream db 
                             (rngs0,rngss1) = (head rngss0, tail rngss0)
                             (result,rngs1) = (head rngs0, tail rngs0)
                             in do put db { random_number_stream_stream=(rngs1:rngss1) }
                                   return (result)

-- |
-- Generates and returns an infinite list of psudo-random integers.
--
dbNextRandomIntegerStream :: DB [Integer]
dbNextRandomIntegerStream = do db <- get
                               let rngss = random_number_stream_stream db
                                   in do put db { random_number_stream_stream=(tail rngss) }
                                         return (head rngss)
