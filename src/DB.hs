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
     dbState,
     dbSetState,
     DBState(..),
     initialDB,
     DB_BaseType,
     dbAddCreature,
     dbAddPlane,
     dbGetCreature,
     dbGetPlane,
     dbModCreature,
     dbModPlane,
     dbMoveInto,
     dbWhere,
     dbGetContents,
     dbGetContentsFiltered,
     dbGetCreatures,
     dbGetCreaturesFiltered,
     dbNextRandomInteger,
     dbNextRandomIntegerStream,
     dbSetStartingRace,
     dbGetStartingRace,
     dbMoveTo)
    where

import DBData
import CreatureData
import PlaneData
import Control.Monad.State
import System.Time
import RNG
import Data.Map as Map
import Data.List as List
import InsidenessMap
import SpeciesData
import Data.Maybe

data DBState = DBRaceSelectionState
	     | DBClassSelectionState Creature
	     | DBPlayerCreatureTurn CreatureRef
	     deriving (Read,Show)

-- |
-- Random access form of the roguestar database.
--
data DB_BaseType = DB_BaseType { db_state :: DBState,
				 db_random_number_stream_stream :: [[Integer]],
				 db_next_object_ref :: Integer,
				 db_starting_race :: Maybe Species,
			         db_creatures :: Map CreatureRef Creature,
				 db_planes :: Map PlaneRef Plane,
				 db_inside :: InsidenessMap DBReference DBReference DBLocation}

-- |
-- Serial form of the roguestar database.
--
data DB_Persistant_BaseType = DB_Persistant_BaseType { db_state_ :: DBState,
						       db_random_number_generator_seed_ :: Integer,
                                                       db_next_object_ref_ :: Integer,
						       db_starting_race_ :: Maybe Species,
						       db_creatures_ :: [(CreatureRef,Creature)],
						       db_planes_ :: [(PlaneRef,Plane)],
						       db_inside_ :: [(DBReference,DBReference,DBLocation)]}
                              deriving (Read,Show)

toPersistant :: DB_BaseType -> DB_Persistant_BaseType
toPersistant db = DB_Persistant_BaseType {
					  db_state_ = db_state db,
					  db_random_number_generator_seed_ = (db_random_number_stream_stream db) !! 0 !! 0,
					  db_next_object_ref_ = db_next_object_ref db,
					  db_starting_race_ = db_starting_race db,
					  db_creatures_ = Map.toList $ db_creatures db,
					  db_planes_ = Map.toList $ db_planes db,
					  db_inside_ = InsidenessMap.toList $ db_inside db
					 }

fromPersistant :: DB_Persistant_BaseType -> DB_BaseType
fromPersistant persistant = DB_BaseType {
					 db_state = db_state_ persistant,
					 db_random_number_stream_stream = randomIntegerStreamStream $ db_random_number_generator_seed_ persistant,
					 db_next_object_ref = db_next_object_ref_ persistant,
					 db_starting_race = db_starting_race_ persistant,
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
initialDB :: IO DB_BaseType
initialDB = do (TOD seconds picos) <- getClockTime
	       return DB_BaseType { db_state = DBRaceSelectionState,
				    db_random_number_stream_stream = randomIntegerStreamStream (seconds + picos),
				    db_next_object_ref = 0,
				    db_starting_race = Nothing,
				    db_creatures = Map.fromList [],
				    db_planes = Map.fromList [],
				    db_inside = InsidenessMap.fromList []
				  }

dbQueryComposable :: (DB_BaseType -> a) -> DB a
dbQueryComposable fn = do db <- get
			  return $ fn db

-- |
-- Returns the DBState of the database.
--
dbState :: DB DBState
dbState = dbQueryComposable db_state

-- |
-- Sets the DBState of the database.
--
dbSetState :: DBState -> DB ()
dbSetState state = do db0 <- get
		      put $ db0 { db_state = state }

-- |
-- Gets the next ObjectRef integer, after incrementing it.
--
dbNextObjectRef :: DB Integer
dbNextObjectRef = do db <- get
		     result <- return $ db_next_object_ref db
		     put (db { db_next_object_ref=(succ result) })
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
-- Modifies an Object based on an ObjectRef.
--
dbModObjectComposable :: DBRef ref => (ref -> DB a) -> ((ref,a) -> DB ()) -> (a -> a) -> ref -> DB ()
dbModObjectComposable get_fn put_fn mod_fn ref =
    do x0 <- get_fn ref
       put_fn (ref,mod_fn x0)

-- |
-- Modifies a Plane based on a PlaneRef.
--
dbModPlane :: (Plane -> Plane) -> PlaneRef -> DB ()
dbModPlane = dbModObjectComposable dbGetPlane dbPutPlane

-- |
-- Modifies a Creature based on a PlaneRef.
--
dbModCreature :: (Creature -> Creature) -> CreatureRef -> DB ()
dbModCreature = dbModObjectComposable dbGetCreature dbPutCreature

-- |
-- Moves the second parameter so that it is inside the first.
--
dbMoveInto :: (DBRef a,DBRef b) => a -> b -> DBLocation -> DB ()
dbMoveInto container item location =
    do db <- get
       imap <- return $ db_inside db
       put $ db { db_inside=InsidenessMap.insert (toDBReference container,toDBReference item,location) imap }

-- |
-- Moves the object within its parent.  (The object has the same parent after the action).
--
dbMoveTo :: (DBRef a) => a -> DBLocation -> DB ()
dbMoveTo item location =
    do where_it_is <- dbWhere item
       case where_it_is of
                  Just (container,_) -> dbMoveInto container item location
                  Nothing -> error "dbMoveTo: but it doesn't have a location to start with"

-- |
-- Returns the (parent,object's location) of this object.
--
dbWhere :: (DBRef a) => a -> DB (Maybe (DBReference,DBLocation))
dbWhere item = liftM (InsidenessMap.lookup (toDBReference item) . db_inside) $ get

-- |
-- Returns the children [(child,child's location)] of this object.
--
dbGetContents :: (DBRef a) => a -> DB [(DBReference,DBLocation)]
dbGetContents item = liftM (Prelude.map (\(_,y,z) -> (y,z)) . InsidenessMap.children (toDBReference item) . db_inside) get

dbGetContentsFiltered :: (DBRef a) => a -> (DBReference -> DB Bool) -> DB [(DBReference,DBLocation)]
dbGetContentsFiltered item fnM = 
    filterM fnM' =<< dbGetContents item
	where fnM' (ref,_) = fnM ref

-- |
-- Returns all the CreatureRefs for Creatures inside this item.
--
dbGetCreatures :: (DBRef a) => a -> DB [(CreatureRef,DBLocation)]
dbGetCreatures item = dbGetCreaturesFiltered item (\_ -> return True)

dbGetCreaturesFiltered :: (DBRef a) => a -> (CreatureRef -> DB Bool) -> DB [(CreatureRef,DBLocation)]
dbGetCreaturesFiltered item fnM =
    liftM (List.map ( \ x -> (toCreatureRef $ fst x,snd x))) $
	  dbGetContentsFiltered item fnM'
	      where fnM' x = if isCreatureRef x
			     then fnM $ toCreatureRef x
			     else return False

-- |
-- Generates and returns the next random Integer.
--
dbNextRandomInteger :: DB Integer
dbNextRandomInteger = do db <- get
			 let rngss0 = db_random_number_stream_stream db 
                             (rngs0,rngss1) = (head rngss0, tail rngss0)
                             (result,rngs1) = (head rngs0, tail rngs0)
                             in do put db { db_random_number_stream_stream=(rngs1:rngss1) }
                                   return (result)

-- |
-- Generates and returns an infinite list of psudo-random integers.
--
dbNextRandomIntegerStream :: DB [Integer]
dbNextRandomIntegerStream = do db <- get
                               let rngss = db_random_number_stream_stream db
                                   in do put db { db_random_number_stream_stream=(tail rngss) }
                                         return (head rngss)

-- |
-- Answers the starting race.
--
dbGetStartingRace :: DB (Maybe Species)
dbGetStartingRace = do db <- get
		       return $ db_starting_race db

-- |
-- Sets the starting race.
--
dbSetStartingRace :: Species -> DB ()
dbSetStartingRace species = do db <- get
			       put $ db { db_starting_race = Just species }