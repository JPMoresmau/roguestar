{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleContexts, Rank2Types, RelaxedPolyRec #-}

module DB
    (DB,
     runDB,
     DBReadable(..),
     playerState,
     setPlayerState,
     PlayerState(..),
     CreatureTurnMode(..),
     SnapshotEvent(..),
     DBError(..),
     CreatureLocation(..),
     ToolLocation(..),
     initial_db,
     DB_BaseType(db_error_flag),
     dbAddCreature,
     dbAddPlane,
     dbAddTool,
     dbUnsafeDeleteObject,
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
     dbSetStartingRace,
     dbGetStartingRace,
     ro, atomic,
     mapRO, filterRO, sortByRO,
     dbGetTimeCoordinate,
     dbAdvanceTime,
     dbNextTurn,
     dbPushSnapshot,
     dbPeepOldestSnapshot,
     dbPopOldestSnapshot,
     dbHasSnapshot,
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
import Control.Monad.Error
import Control.Monad.Reader
import TimeCoordinate
import Data.Ord
import Control.Arrow (first)

data PlayerState = 
    RaceSelectionState
  | ClassSelectionState Creature
  | PlayerCreatureTurn CreatureRef CreatureTurnMode
  | SnapshotEvent SnapshotEvent
  | GameOver
	     deriving (Read,Show)

data CreatureTurnMode =
    NormalMode
  | PickupMode
  | DropMode
  | WieldMode
      deriving (Read,Show)

data SnapshotEvent = 
    AttackEvent {
        attack_event_source_creature :: CreatureRef,
        attack_event_source_weapon :: Maybe ToolRef,
        attack_event_target_creature :: CreatureRef }
  | MissEvent {
        miss_event_creature :: CreatureRef,
	miss_event_weapon :: Maybe ToolRef }
  | KilledEvent {
        killed_event_creature :: CreatureRef }
            deriving (Read,Show)

data DB_History = DB_History {
    db_here :: DB_BaseType,
    db_random :: [[Integer]] }

data DB_BaseType = DB_BaseType { db_player_state :: PlayerState,
				 db_next_object_ref :: Integer,
				 db_starting_race :: Maybe Species,
			         db_creatures :: Map CreatureRef Creature,
				 db_planes :: Map PlaneRef Plane,
				 db_tools :: Map ToolRef Tool,
				 db_hierarchy :: HierarchicalDatabase (Location S (Reference ()) ()),
				 db_time_coordinates :: Map (Reference ()) TimeCoordinate,
				 db_error_flag :: String,
				 db_prior_snapshot :: Maybe DB_BaseType}
    deriving (Read,Show)

data DBError =
    DBError String
  | DBErrorFlag String
    deriving (Read,Show)

instance Error DBError where
    strMsg = DBError

newtype DB a = DB (ErrorT DBError (State DB_History) a)

runDB :: DB a -> DB_BaseType -> IO (Either DBError (a,DB_BaseType))
runDB (DB actionM) db = 
    do hist <- setupDBHistory db
       return $ case runState (runErrorT actionM) hist of
                    (Right a,DB_History { db_here = db' }) -> Right (a,db')
	            (Left e,_) -> Left e

instance Monad DB where
    (DB k) >>= m = DB $ k >>= (\x -> let DB n = m x in n)
    return = DB . return
    fail s = DB $ throwError $ DBError $ "engine-error: " ++ s

instance MonadState DB_BaseType DB where
    get = liftM db_here $ DB get
    put s = DB $ modify (\x -> x { db_here = s })

instance MonadReader DB_BaseType DB where
    ask = liftM db_here $ DB get
    local f actionM = 
        do s <- get
	   modify f
           a <- catchError (liftM Right actionM) (return . Left)
	   put s
           either throwError return a

instance MonadError DBError DB where
    throwError = DB . throwError
    catchError (DB actionM) handlerM = DB $ catchError actionM (\e -> let DB n = handlerM e in n)

class (Monad db,MonadError DBError db,MonadReader DB_BaseType db) => DBReadable db where
    dbNextRandomInteger :: db Integer
    dbNextRandomIntegerStream :: db [Integer]
    dbSimulate :: DB a -> db a
    dbPeepSnapshot :: (DBReadable db) => (forall m. DBReadable m => m a) -> db (Maybe a)

instance DBReadable DB where
    dbNextRandomInteger = 
        do db <- DB get
	   let rngss0 = db_random db 
               (rngs0,rngss1) = (head rngss0, tail rngss0)
               (result,rngs1) = (head rngs0, tail rngs0)
           DB $ put db { db_random=(rngs1:rngss1) }
           return (result)
    dbNextRandomIntegerStream = 
        do db <- DB get
           let rngss = db_random db
           DB $ put db { db_random=(tail rngss) }
           return (head rngss)
    dbSimulate = local id
    dbPeepSnapshot actionM =
        do s <- DB $ gets db_here
	   m_snapshot <- gets db_prior_snapshot
	   case m_snapshot of
	       Just snapshot ->
	           do DB $ modify $ \hist -> hist { db_here = snapshot }
	              a <- dbSimulate actionM
                      DB $ modify $ \hist -> hist { db_here = s }
	              return $ Just a
               Nothing ->  return Nothing
	   

ro :: (DBReadable db) => (forall m. DBReadable m => m a) -> db a
ro db = dbSimulate db

filterRO :: (DBReadable db) => (forall m. DBReadable m => a -> m Bool) -> [a] -> db [a]
filterRO f xs = ro $ filterM f xs

mapRO :: (DBReadable db) => (forall m. DBReadable m => a -> m b) -> [a] -> db [b]
mapRO f xs = ro $ mapM f xs

sortByRO :: (DBReadable db,Ord b) => (forall m. DBReadable m => a -> m b) -> [a] -> db [a]
sortByRO f xs =
    liftM (List.map fst . sortBy (comparing snd)) $ flip mapRO xs $ \x -> 
         do y <- f x
	    return (x,y)

atomic :: (forall m. DBReadable m => m (DB a)) -> DB a
atomic transaction = 
    do db_a <- ro transaction
       (a,s) <- dbSimulate $
           do a <- db_a
	      s <- get
              return (a,s)
       put s
       return a

-- |
-- Generates an initial DB state.
--
initial_db :: DB_BaseType
initial_db = DB_BaseType { 
    db_player_state = RaceSelectionState,
    db_next_object_ref = 0,
    db_starting_race = Nothing,
    db_creatures = Map.fromList [],
    db_planes = Map.fromList [],
    db_tools = Map.fromList [],
    db_hierarchy = HierarchicalDatabase.fromList [],
    db_error_flag = [],
    db_time_coordinates = Map.fromList [(generalizeReference the_universe, zero_time)],
    db_prior_snapshot = Nothing }

setupDBHistory :: DB_BaseType -> IO DB_History
setupDBHistory db =
    do (TOD seconds picos) <- getClockTime
       return $ DB_History {
           db_here = db,
	   db_random = randomIntegerStreamStream (seconds + picos) }

-- |
-- Returns the DBState of the database.
--
playerState :: (DBReadable m) => m PlayerState
playerState = asks db_player_state

-- |
-- Sets the DBState of the database.
--
setPlayerState :: PlayerState -> DB ()
setPlayerState state = modify (\db -> db { db_player_state = state })

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
dbAddObjectComposable :: (ReferenceType a,LocationType (Reference a),LocationType l) =>
                         (Integer -> (Reference a)) -> 
                         (Reference a -> a -> DB ()) -> 
                         (Reference a -> l -> Location S (Reference a) l) -> 
                         a -> l -> DB (Reference a)
dbAddObjectComposable constructReference updateObject constructLocation thing loc = 
    do ref <- liftM constructReference $ dbNextObjectRef
       updateObject ref thing
       dbSetLocation $ constructLocation ref loc
       parent_ref <- liftM (getLocation) $ dbWhere ref
       dbSetTimeCoordinate (generalizeReference ref) =<< dbGetTimeCoordinate (generalizeReference parent_ref)
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
-- This deletes an object, but leaves any of it's contents dangling.
--
dbUnsafeDeleteObject :: (ReferenceType e) =>
        (forall m. DBReadable m => 
         Location M (Reference ()) (Reference e) -> 
	 m (Location M (Reference ()) ())) ->
    Reference e -> 
    DB ()
dbUnsafeDeleteObject f ref =
    do dbMoveAllWithin f ref
       modify $ \db -> db {
           db_creatures = Map.delete (unsafeReference ref) $ db_creatures db,
           db_planes = Map.delete (unsafeReference ref) $ db_planes db,
           db_tools = Map.delete (unsafeReference ref) $ db_tools db,
           db_hierarchy = HierarchicalDatabase.delete (toUID ref)  $ db_hierarchy db,
           db_time_coordinates = Map.delete (generalizeReference ref) $ db_time_coordinates db }

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
    asks (fromMaybe (error "dbGetObjectComposable: Nothing") . Map.lookup ref . get_fn)

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
dbSetLocation :: (LocationType e,LocationType t) => Location S e t -> DB ()
dbSetLocation loc = 
    do case (fmap location $ coerceLocationTyped _wielded loc) of
           Just (Wielded c) -> dbUnwieldCreature c
	   Nothing -> return ()
       modify (\db -> db { db_hierarchy=HierarchicalDatabase.insert (unsafeLocation loc) $ db_hierarchy db })

-- |
-- Shunt any wielded objects into inventory.
--
dbUnwieldCreature :: CreatureRef -> DB ()
dbUnwieldCreature c = mapM_ (dbSetLocation . returnToInventory) =<< dbGetContents c

-- |
-- Moves an object, returning the location of the object before and after
-- the move.
--
dbMove :: (LocationType (Reference e),LocationType b) =>
          (forall m. DBReadable m => Location M (Reference e) () -> m (Location M (Reference e) b)) -> 
          (Reference e) ->
          DB (Location S (Reference e) (),Location S (Reference e) b)
dbMove moveF ref =
    do old <- dbWhere ref
       new <- ro $ moveF (unsafeLocation old)
       dbSetLocation $ generalizeLocationRecord $ unsafeLocation new
       return (unsafeLocation old, unsafeLocation new)

dbMoveAllWithin :: (forall m. DBReadable m => 
                       Location M (Reference ()) (Reference e) ->
		       m (Location M (Reference ()) ())) ->
                   Reference e ->
		   DB [(Location S (Reference ()) (Reference e),Location S (Reference ()) ())]
dbMoveAllWithin f ref = mapM (liftM (first unsafeLocation) . dbMove (f . unsafeLocation)) =<< dbGetContents ref

-- |
-- Verifies that a reference is in the database.
--
dbVerify :: (DBReadable db) => Reference e -> db Bool
dbVerify ref = asks (isJust . HierarchicalDatabase.parentOf (toUID ref) . db_hierarchy)

-- |
-- Returns the location of this object.
--
dbWhere :: (DBReadable db) => Reference e -> db (Location S (Reference e) ())
dbWhere item = asks (unsafeLocation . fromMaybe (error "dbWhere: has no location") .
                       HierarchicalDatabase.lookupParent (toUID item) . db_hierarchy)

-- |
-- Returns all ancestor Locations of this element starting with the location
-- of the element and ending with theUniverse.
--
dbGetAncestors :: (DBReadable db,ReferenceType e) => Reference e -> db [Location S (Reference ()) ()]
dbGetAncestors ref | isReferenceTyped _the_universe ref = return []
dbGetAncestors ref =
    do this <- dbWhere $ generalizeReference ref
       rest <- dbGetAncestors $ getLocation this
       return $ this : rest

-- |
-- Returns the location records of this object.
--
dbGetContents :: (DBReadable db,GenericReference a S) => Reference t -> db [a]
dbGetContents item = asks (Data.Maybe.mapMaybe fromLocation . HierarchicalDatabase.lookupChildren 
                               (toUID item) . db_hierarchy)

-- |
-- Gets the time of an object.
-- 
dbGetTimeCoordinate :: (DBReadable db,ReferenceType a) => Reference a -> db TimeCoordinate
dbGetTimeCoordinate ref = asks (fromMaybe (error "dbGetTimeCoordinate: missing time coordinate.") . 
                                  Map.lookup (generalizeReference ref) . db_time_coordinates)

-- |
-- Sets the time of an object.
--
dbSetTimeCoordinate :: (ReferenceType a) => Reference a -> TimeCoordinate -> DB ()
dbSetTimeCoordinate ref tc = modify (\db -> db { db_time_coordinates = Map.insert (generalizeReference ref) tc $ db_time_coordinates db })

-- |
-- Advances the time of an object.
--
dbAdvanceTime :: (ReferenceType a) => Rational -> Reference a -> DB ()
dbAdvanceTime t ref = dbSetTimeCoordinate ref =<< (return . (advanceTime t)) =<< dbGetTimeCoordinate ref

-- |
-- Finds the object whose turn is next, among a restricted group of objects.
--
dbNextTurn :: (DBReadable db,ReferenceType a) => [Reference a] -> db (Reference a)
dbNextTurn [] = error "dbNextTurn: empty list"
dbNextTurn refs =
    asks (\db -> fst $ minimumBy (comparing snd) $
                   List.map (\r -> (r,fromMaybe (error "dbNextTurn: missing time coordinate") $
                                      Map.lookup (generalizeReference r) (db_time_coordinates db))) refs)

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

-- |
-- Takes a snapshot of a DBEvent in progress.
--
dbPushSnapshot :: SnapshotEvent -> DB ()
dbPushSnapshot e = modify $ \db -> db {
    db_prior_snapshot = Just $ db { db_player_state = SnapshotEvent e } }

dbPeepOldestSnapshot :: (DBReadable db) => (forall m. DBReadable m => m a) -> db a
dbPeepOldestSnapshot actionM =
    do m_a <- dbPeepSnapshot $ dbPeepOldestSnapshot actionM
       maybe actionM return m_a

dbPopOldestSnapshot :: DB ()
dbPopOldestSnapshot = modify popOldestSnapshot

dbHasSnapshot :: (DBReadable db) => db Bool
dbHasSnapshot = liftM isJust $ dbPeepSnapshot (return ())

popOldestSnapshot :: DB_BaseType -> DB_BaseType
popOldestSnapshot db = 
    case isJust $ db_prior_snapshot =<< db_prior_snapshot db of
        False -> db { db_prior_snapshot = Nothing }
	True  -> db { db_prior_snapshot = fmap popOldestSnapshot $ db_prior_snapshot db }
