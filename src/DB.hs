{-# OPTIONS_GHC -fglasgow-exts #-}

module DB
    (DB,
     dbState,
     dbSetState,
     DBState(..),
     DBReadable(..),
     DBSpecialMode(..),
     DBError(..),
     CreatureLocation(..),
     ToolLocation(..),
     initialDB,
     DB_BaseType(db_error_flag),
     dbAddCreature,
     dbAddPlane,
     dbAddTool,
     dbGetCreature,
     dbGetPlane,
     dbGetTool,
     dbModCreature,
     dbModPlane,
     dbModTool,
     dbMove,
     dbUnwieldCreature,
     dbVerify,
     dbGetAncestors,
     dbWhere,
     dbGetContents,
     dbNextRandomInteger,
     dbNextRandomIntegerStream,
     dbSetStartingRace,
     dbGetStartingRace,
     ro,
     mapRO, filterRO,
     dbSimulate,
     module DBData)
    where

import DBPrivate
import DBData
import CreatureData
import PlaneData
import System.Time
import RNG
import Data.Map as Map
import Data.List as List
import HierarchicalDatabase
import SpeciesData
import Data.Maybe
import ToolData
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

data DBState = DBRaceSelectionState
	     | DBClassSelectionState Creature
	     | DBPlayerCreatureTurn CreatureRef DBSpecialMode
	     deriving (Read,Show)

data DBSpecialMode =
    DBNotSpecial
  | DBPickupMode
  | DBDropMode
  | DBWieldMode
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
				 db_tools :: Map ToolRef Tool,
				 db_hierarchy :: HierarchicalDatabase (Location () () ()),
				 db_error_flag :: String }

-- |
-- Serial form of the roguestar database.
--
data DB_Persistant_BaseType = DB_Persistant_BaseType { db_state_ :: DBState,
						       db_random_number_generator_seed_ :: Integer,
                                                       db_next_object_ref_ :: Integer,
						       db_starting_race_ :: Maybe Species,
						       db_creatures_ :: [(CreatureRef,Creature)],
						       db_planes_ :: [(PlaneRef,Plane)],
						       db_tools_ :: [(ToolRef,Tool)],
						       db_hierarchy_ :: [Location () () ()]}
                              deriving (Read,Show)

toPersistant :: DB_BaseType -> DB_Persistant_BaseType
toPersistant db = DB_Persistant_BaseType {
					  db_state_ = db_state db,
					  db_random_number_generator_seed_ = (db_random_number_stream_stream db) !! 0 !! 0,
					  db_next_object_ref_ = db_next_object_ref db,
					  db_starting_race_ = db_starting_race db,
					  db_creatures_ = Map.toList $ db_creatures db,
					  db_planes_ = Map.toList $ db_planes db,
					  db_tools_ = Map.toList $ db_tools db,
					  db_hierarchy_ = HierarchicalDatabase.toList $ db_hierarchy db
					 }

fromPersistant :: DB_Persistant_BaseType -> DB_BaseType
fromPersistant persistant = DB_BaseType {
					 db_state = db_state_ persistant,
					 db_random_number_stream_stream = randomIntegerStreamStream $ db_random_number_generator_seed_ persistant,
					 db_next_object_ref = db_next_object_ref_ persistant,
					 db_starting_race = db_starting_race_ persistant,
					 db_creatures = Map.fromList $ db_creatures_ persistant,
					 db_planes = Map.fromList $ db_planes_ persistant,
					 db_tools = Map.fromList $ db_tools_ persistant,
					 db_hierarchy = HierarchicalDatabase.fromList $ db_hierarchy_ persistant,
					 db_error_flag = []
					}

fromPersistant_tupled :: (DB_Persistant_BaseType,String) -> (DB_BaseType,String)
fromPersistant_tupled (persistant,str) = (fromPersistant persistant,str)

instance Read DB_BaseType where
    readsPrec n = \x -> Prelude.map fromPersistant_tupled (readsPrec n x)

instance Show DB_BaseType where
    show db = show (toPersistant db)

data DBError =
    DBError String
  | DBErrorFlag String
    deriving (Read,Show)

instance Error DBError where
    strMsg = DBError

type DB a = ErrorT DBError (State DB_BaseType) a

type DBRO a = ErrorT DBError (Reader DB_BaseType) a

class (MonadError DBError m,Monad m) => DBReadable m where
    dbget :: m DB_BaseType

instance DBReadable (ErrorT DBError (State DB_BaseType)) where
    dbget = get

instance DBReadable (ErrorT DBError (Reader DB_BaseType)) where
    dbget = ask

ro :: (DBReadable db) => DBRO a -> db a
ro actionM = 
    do result <- dbgets (runReader $ runErrorT actionM)
       case result of
           Right x -> return x
	   Left err -> throwError err

filterRO :: (DBReadable db) => (a -> DBRO Bool) -> [a] -> db [a]
filterRO f xs = ro $ filterM f xs

mapRO :: (DBReadable db) => (a -> DBRO b) -> [a] -> db [b]
mapRO f xs = ro $ mapM f xs

dbSimulate :: (DBReadable db) => DB a -> db (Either DBError a)
dbSimulate dbAction = liftM (evalState $ runErrorT dbAction) $ dbgets id 

dbgets :: (DBReadable m) => (DB_BaseType -> a) -> m a
dbgets f = liftM f dbget

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
				    db_tools = Map.fromList [],
				    db_hierarchy = HierarchicalDatabase.fromList [],
				    db_error_flag = []
				  }

-- |
-- Returns the DBState of the database.
--
dbState :: (DBReadable m) => m DBState
dbState = dbgets db_state

-- |
-- Sets the DBState of the database.
--
dbSetState :: DBState -> DB ()
dbSetState state = modify (\db -> db { db_state = state })

-- |
-- Gets the next ObjectRef integer, after incrementing it.
--
dbNextObjectRef :: DB Integer
dbNextObjectRef = do modify (\db -> db { db_next_object_ref = succ $ db_next_object_ref db })
                     gets db_next_object_ref

class (LocationType l) => CreatureLocation l where
    creatureLocation :: CreatureRef -> l -> Location m CreatureRef l

class (LocationType l) => ToolLocation l where
    toolLocation :: ToolRef -> l -> Location m ToolRef l

instance CreatureLocation Standing where
    creatureLocation a l = IsStanding (unsafeReference a) l

instance ToolLocation Dropped where
    toolLocation a l = IsDropped (unsafeReference a) l

instance ToolLocation Inventory where
    toolLocation a l = InInventory (unsafeReference a) l

instance ToolLocation Wielded where
    toolLocation a l = IsWielded (unsafeReference a) l

-- |
-- Adds something to a map in the database using a new object reference.
--
dbAddObjectComposable :: (LocationType (Reference a),LocationType l) =>
                         (Integer -> (Reference a)) -> 
                         (Reference a -> a -> DB ()) -> 
                         (Reference a -> l -> Location () (Reference a) l) -> 
                         a -> l -> DB (Reference a)
dbAddObjectComposable constructReference updateObject constructLocation thing loc = 
    do ref <- liftM constructReference $ dbNextObjectRef
       updateObject ref thing
       dbSetLocation $ constructLocation ref loc
       return ref

-- |
-- Adds a new Creature to the database.
--
dbAddCreature :: (CreatureLocation l) => Creature -> l -> DB CreatureRef
dbAddCreature = dbAddObjectComposable CreatureRef dbPutCreature creatureLocation

-- |
-- Adds a new Plane to the database.
--
dbAddPlane :: Plane -> () -> DB PlaneRef
dbAddPlane = dbAddObjectComposable PlaneRef dbPutPlane (\a () -> InTheUniverse a)

-- |
-- Adds a new Tool to the database.
--
dbAddTool :: (ToolLocation l) => Tool -> l -> DB ToolRef
dbAddTool = dbAddObjectComposable ToolRef dbPutTool toolLocation

-- |
-- Puts an object into the database using getter and setter functions.
--
dbPutObjectComposable :: (Ord a) => (DB_BaseType -> Map a b) -> 
                                    (Map a b -> DB_BaseType -> DB_BaseType) -> 
                                    a -> b -> 
                                    DB ()
dbPutObjectComposable get_map_fn put_map_fn key thing = 
    modify (\db -> put_map_fn (Map.insert key thing $ get_map_fn db) db)

-- |
-- Puts a Creature under an arbitrary CreatureRef.
--
dbPutCreature :: CreatureRef -> Creature -> DB ()
dbPutCreature = dbPutObjectComposable db_creatures (\x db_base_type -> db_base_type { db_creatures = x })

-- |
-- Puts a Plane under an arbitrary PlaneRef
--
dbPutPlane :: PlaneRef -> Plane -> DB ()
dbPutPlane = dbPutObjectComposable db_planes (\x db_base_type -> db_base_type { db_planes = x })

-- |
-- Puts a Tool under an arbitrary ToolRef
--
dbPutTool :: ToolRef -> Tool -> DB ()
dbPutTool = dbPutObjectComposable db_tools (\x db_base_type -> db_base_type { db_tools = x })

-- |
-- Gets an object from the database using getter functions.
--
dbGetObjectComposable :: (DBReadable db,Ord a) => (DB_BaseType -> Map a b) -> a -> db b
dbGetObjectComposable get_fn ref = 
    dbgets (fromMaybe (error "dbGetObjectComposable: Nothing") . Map.lookup ref . get_fn)

-- |
-- Gets a Creature from a CreatureRef
--
dbGetCreature :: (DBReadable m) => CreatureRef -> m Creature
dbGetCreature = dbGetObjectComposable db_creatures

-- |
-- Gets a Plane from a PlaneRef
--
dbGetPlane :: (DBReadable m) => PlaneRef -> m Plane
dbGetPlane = dbGetObjectComposable db_planes

-- |
-- Gets a Plane from a PlaneRef
--
dbGetTool :: (DBReadable m) => ToolRef -> m Tool
dbGetTool = dbGetObjectComposable db_tools

-- |
-- Modifies an Object based on an ObjectRef.
--
dbModObjectComposable :: (Reference e -> DB e) -> (Reference e -> e -> DB ()) -> 
                         (e -> e) -> Reference e -> DB ()
dbModObjectComposable getter putter f ref = (putter ref . f) =<< (getter ref)

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
-- Modifies a Tool based on a PlaneRef.
--
dbModTool :: (Tool -> Tool) -> ToolRef -> DB ()
dbModTool = dbModObjectComposable dbGetTool dbPutTool

-- |
-- Set the location of an object.
--
dbSetLocation :: (LocationType e,LocationType t) => Location m e t -> DB ()
dbSetLocation loc = 
    do case (fmap location $ toWieldedLocation loc) of
           Just (Wielded c) -> dbUnwieldCreature c
	   Nothing -> return ()
       modify (\db -> db { db_hierarchy=HierarchicalDatabase.insert (unsafeLocation loc) $ db_hierarchy db })

-- |
-- Shunt any wielded objects into inventory.
--
dbUnwieldCreature :: CreatureRef -> DB ()
dbUnwieldCreature c =
    do cs_tools <- dbGetContents c
       mapM_ (maybe (return ()) (dbSetLocation . unwieldTool) . coerceLocation) cs_tools

-- |
-- Moves an object, returning the location of the object before and after
-- the move.
--
dbMove :: (LocationType (Reference e),LocationType b) =>
          (Location M (Reference e) () -> DBRO (Location M (Reference e) b)) -> 
          (Reference e) ->
          DB (Location S e (),Location S e b)
dbMove moveF ref =
    do old <- dbWhere ref
       new <- ro $ moveF (unsafeLocation old)
       dbSetLocation new
       return (unsafeLocation old, unsafeLocation new)

-- |
-- Verifies that a reference is in the database.
--
dbVerify :: (DBReadable db) => Reference e -> db Bool
dbVerify ref = dbgets (isJust . HierarchicalDatabase.parentOf (toUID ref) . db_hierarchy)

-- |
-- Returns the location of this object.
--
dbWhere :: (DBReadable db) => Reference e -> db (Location S (Reference e) ())
dbWhere item = dbgets (unsafeLocation . fromMaybe (error "dbWhere: has no location") .
                       HierarchicalDatabase.lookupParent (toUID item) . db_hierarchy)

-- |
-- Returns all ancestor Locations of this element starting with the location
-- of the element and ending with theUniverse.
--
dbGetAncestors :: (DBReadable db,ReferenceType e) => Reference e -> db [Location S () ()]
dbGetAncestors ref | isTheUniverse ref = return []
dbGetAncestors ref =
    do this <- liftM unsafeLocation $ dbWhere ref
       rest <- dbGetAncestors $ genericParent this
       return $ this : rest

-- |
-- Returns the location records of this object.
--
dbGetContents :: (DBReadable db) => Reference t -> db [Location S () (Reference t)]
dbGetContents item = dbgets (List.map unsafeLocation . HierarchicalDatabase.lookupChildren 
                               (toUID item) . db_hierarchy)

-- |
-- Generates and returns the next random Integer.
--
dbNextRandomInteger :: DB Integer
dbNextRandomInteger = do db <- get
			 let rngss0 = db_random_number_stream_stream db 
                             (rngs0,rngss1) = (head rngss0, tail rngss0)
                             (result,rngs1) = (head rngs0, tail rngs0)
                         put db { db_random_number_stream_stream=(rngs1:rngss1) }
                         return (result)

-- |
-- Generates and returns an infinite list of psudo-random integers.
--
dbNextRandomIntegerStream :: DB [Integer]
dbNextRandomIntegerStream = do db <- get
                               let rngss = db_random_number_stream_stream db
                               put db { db_random_number_stream_stream=(tail rngss) }
                               return (head rngss)

-- |
-- Answers the starting race.
--
dbGetStartingRace :: DB (Maybe Species)
dbGetStartingRace = do gets db_starting_race

-- |
-- Sets the starting race.
--
dbSetStartingRace :: Species -> DB ()
dbSetStartingRace species = modify (\db -> db { db_starting_race = Just species })
