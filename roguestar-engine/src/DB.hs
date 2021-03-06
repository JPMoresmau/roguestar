{-# LANGUAGE MultiParamTypeClasses,
             ExistentialQuantification,
             FlexibleContexts,
             Rank2Types,
             RelaxedPolyRec,
             ScopedTypeVariables #-}

module DB
    (DBResult,
     DB,
     runDB,
     DBReadable(..),
     playerState,
     setPlayerState,
     SnapshotEvent(..),
     DBError(..),
     CreatureLocation(..),
     ToolLocation(..),
     PlaneLocation(..),
     initial_db,
     DB_BaseType(db_error_flag),
     dbActionCount,
     dbAddCreature,
     dbAddPlane,
     dbAddTool,
     dbAddBuilding,
     dbUnsafeDeleteObject,
     dbGetCreature,
     dbGetPlane,
     dbGetTool,
     dbGetBuilding,
     dbModCreature,
     dbModPlane,
     dbModTool,
     dbModBuilding,
     dbMove,
     dbUnwieldCreature,
     dbVerify,
     dbGetAncestors,
     dbWhere,
     dbGetContents,
     dbSetStartingSpecies,
     dbGetStartingSpecies,
     ro, atomic,
     logDB,
     mapRO, filterRO, sortByRO,
     dbGetTimeCoordinate,
     dbAdvanceTime,
     dbNextTurn,
     dbPushSnapshot,
     dbPeepOldestSnapshot,
     dbPopOldestSnapshot,
     dbHasSnapshot,
     module DBData,
     module DBErrorFlag,
     module Random)
    where

import DBPrivate
import DBData
import CreatureData
import PlaneData
import BuildingData
import RNG
import Data.Map as Map
import Data.List as List
import qualified HierarchicalDatabase as HD
import SpeciesData
import Data.Maybe
import ToolData
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import TimeCoordinate
import Data.Ord
import Control.Arrow (first,second)
import Control.Monad.Random as Random
import Random
import PlayerState
import DBErrorFlag
import Control.Parallel.Strategies
import System.IO.Unsafe
import Logging

data DB_History = DB_History {
    db_here :: DB_BaseType,
    db_random :: RNG }

data DB_BaseType = DB_BaseType { db_player_state :: PlayerState,
                                 db_next_object_ref :: Integer,
                                 db_starting_species :: Maybe Species,
                                 db_creatures :: Map CreatureRef Creature,
                                 db_planes :: Map PlaneRef Plane,
                                 db_tools :: Map ToolRef Tool,
                                 db_buildings :: Map BuildingRef Building,
                                 db_hierarchy :: HD.HierarchicalDatabase (Location (Reference ()) ()),
                                 db_time_coordinates :: Map (Reference ()) TimeCoordinate,
                                 db_error_flag :: String,
                                 db_prior_snapshot :: Maybe DB_BaseType,
                                 db_action_count :: Integer }
    deriving (Read,Show)

data DBError =
    DBError String
  | DBErrorFlag ErrorFlag
    deriving (Read,Show)

instance Error DBError where
    strMsg = DBError

type DBResult r = Either DBError (r,DB_History)
data DB a = DB { cycleDB :: forall r. DB_History -> (a -> DB_History -> DBResult r) -> DBResult r }

runDB :: DB a -> DB_BaseType -> IO (Either DBError (a,DB_BaseType))
runDB dbAction database =
    do hist <- setupDBHistory database
       return $ (either Left (Right . second db_here)) $ cycleDB dbAction hist $ \a h -> Right (a,h)

instance Monad DB where
    return a = DB $ \h f -> f a h
    k >>= m = DB $ \h f -> cycleDB k h $ \a h' -> cycleDB (m a) h' f
    fail = error

instance Functor DB where
    fmap = liftM

instance Applicative DB where
    pure = return
    (<*>) = ap

instance MonadState DB_BaseType DB where
    get = DB $ \h f -> f (db_here h) h
    put s = DB $ \h f -> f () $ modification h
        where modification = \db -> db { db_here = s { db_action_count = succ $ db_action_count $ db_here db } }

instance MonadReader DB_BaseType DB where
    ask = get
    local modification actionM =
        do split_rng <- dbRandomSplit
           s <- get
           modify modification
           a <- catchError (liftM Right actionM) (return . Left)
           DB $ \h f -> f () $ h { db_here = s, db_random = split_rng }
           either throwError return a

instance MonadError DBError DB where
    throwError e = DB $ \_ _ -> Left e
    catchError actionM handlerM = DB $ \h f -> either (\err -> cycleDB (handlerM err) h f) Right $ cycleDB actionM h f

instance MonadRandom DB where
    getRandom = dbRandom random
    getRandoms = liftM randoms $ dbRandom Random.split
    getRandomR min_max = dbRandom $ randomR min_max
    getRandomRs min_max = liftM (randomRs min_max) $ dbRandom Random.split

dbRandom :: (RNG -> (a,RNG)) -> DB a
dbRandom rgen = DB $ \h f -> let (x,g) = rgen (db_random h) in f x (h { db_random = g })

dbRandomSplit :: DB RNG
dbRandomSplit = DB $ \h f -> let (a,b) = Random.split (db_random h) in f a (h { db_random = b })

class (Monad db,MonadError DBError db,MonadReader DB_BaseType db,MonadRandom db,Applicative db) => DBReadable db where
    dbSimulate :: DB a -> db a
    dbPeepSnapshot :: (DBReadable db) => (forall m. DBReadable m => m a) -> db (Maybe a)

instance DBReadable DB where
    dbSimulate = local id
    dbPeepSnapshot actionM =
        do s <- DB $ \h f -> f (db_here h) h
           m_snapshot <- gets db_prior_snapshot
           case m_snapshot of
               Just snapshot ->
                   do split_rng <- dbRandomSplit
                      DB $ \h f -> f () $ h { db_here = snapshot }
                      a <- dbSimulate actionM
                      DB $ \h f -> f () $ h { db_here = s, db_random = split_rng }
                      return $ Just a
               Nothing ->  return Nothing

logDB :: (DBReadable db) => String -> Priority -> String -> db ()
logDB l p s = return $! unsafePerformIO $ logM l p s

ro :: (DBReadable db) => (forall m. DBReadable m => m a) -> db a
ro db = dbSimulate db

filterRO :: (DBReadable db) => (forall m. DBReadable m => a -> m Bool) -> [a] -> db [a]
filterRO f xs = liftM (`using` parList rwhnf) $ filterM (dbSimulate . f) xs

mapRO :: (DBReadable db) => (forall m. DBReadable m => a -> m b) -> [a] -> db [b]
mapRO f xs = liftM (`using` parList rwhnf) $ mapM (dbSimulate . f) xs

sortByRO :: (DBReadable db,Ord b) => (forall m. DBReadable m => a -> m b) -> [a] -> db [a]
sortByRO f xs =
    liftM (List.map fst . sortBy (comparing snd)) $ mapRO (\x ->
         do y <- f x
            return (x,y)) xs

-- | Run action synthesized from a read-only action (prepare-execute pattern).
atomic :: (x -> DB ()) -> (forall m. DBReadable m => m x) -> DB x
atomic action ro_action =
    do x <- ro ro_action
       s <- dbSimulate $
           do action x
              s <- get
              return s
       put s
       return x

-- |
-- Generates an initial DB state.
--
initial_db :: DB_BaseType
initial_db = DB_BaseType {
    db_player_state = SpeciesSelectionState,
    db_next_object_ref = 0,
    db_starting_species = Nothing,
    db_creatures = Map.fromList [],
    db_planes = Map.fromList [],
    db_tools = Map.fromList [],
    db_buildings = Map.fromList [],
    db_hierarchy = HD.fromList [],
    db_error_flag = [],
    db_time_coordinates = Map.fromList [(generalizeReference the_universe, zero_time)],
    db_prior_snapshot = Nothing,
    db_action_count = 0 }

setupDBHistory :: DB_BaseType -> IO DB_History
setupDBHistory db =
    do rng <- randomIO
       return $ DB_History {
           db_here = db,
           db_random = rng }

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

dbActionCount :: (DBReadable db) => db Integer
dbActionCount = asks db_action_count

-- |
-- Gets the next ObjectRef integer, after incrementing it.
--
dbNextObjectRef :: DB Integer
dbNextObjectRef = do modify $ \db -> db { db_next_object_ref = succ $ db_next_object_ref db }
                     gets db_next_object_ref

class (LocationParent l) => CreatureLocation l where
    creatureLocation :: CreatureRef -> l -> Location CreatureRef l

class (LocationParent l) => ToolLocation l where
    toolLocation :: ToolRef -> l -> Location ToolRef l

class (LocationParent l) => BuildingLocation l where
    buildingLocation :: BuildingRef -> l -> Location BuildingRef l

class (LocationParent l) => PlaneLocation l where
    planeLocation :: PlaneRef -> l -> Location PlaneRef l

instance CreatureLocation Standing where
    creatureLocation a l = IsStanding (unsafeReference a) l

instance ToolLocation Dropped where
    toolLocation a l = IsDropped (unsafeReference a) l

instance ToolLocation Inventory where
    toolLocation a l = InInventory (unsafeReference a) l

instance ToolLocation Wielded where
    toolLocation a l = IsWielded (unsafeReference a) l

instance BuildingLocation Constructed where
    buildingLocation a l = IsConstructed (unsafeReference a) l

instance PlaneLocation TheUniverse where
    planeLocation a _ = InTheUniverse a

instance PlaneLocation Subsequent where
    planeLocation a l = IsSubsequent a l

instance PlaneLocation Beneath where
    planeLocation a l = IsBeneath a l

-- |
-- Adds something to a map in the database using a new object reference.
--
dbAddObjectComposable :: (ReferenceType a,
                          LocationChild (Reference a),
                          LocationParent l) =>
                         (Integer -> (Reference a)) ->
                         (Reference a -> a -> DB ()) ->
                         (Reference a -> l -> Location (Reference a) l) ->
                         a -> l -> DB (Reference a)
dbAddObjectComposable constructReference updateObject constructLocation thing loc =
    do ref <- liftM constructReference $ dbNextObjectRef
       updateObject ref thing
       dbSetLocation $ constructLocation ref loc
       genericParent_ref <- liftM genericParent $ dbWhere ref
       dbSetTimeCoordinate (generalizeReference ref) =<< dbGetTimeCoordinate (generalizeReference genericParent_ref)
       return ref

-- |
-- Adds a new Creature to the database.
--
dbAddCreature :: (CreatureLocation l) => Creature -> l -> DB CreatureRef
dbAddCreature = dbAddObjectComposable CreatureRef dbPutCreature creatureLocation

-- |
-- Adds a new Plane to the database.
--
dbAddPlane :: (PlaneLocation l) => Plane -> l -> DB PlaneRef
dbAddPlane = dbAddObjectComposable PlaneRef dbPutPlane planeLocation

-- |
-- Adds a new Tool to the database.
--
dbAddTool :: (ToolLocation l) => Tool -> l -> DB ToolRef
dbAddTool = dbAddObjectComposable ToolRef dbPutTool toolLocation

-- |
-- Adds a new Tool to the database.
--
dbAddBuilding :: (BuildingLocation l) => Building -> l -> DB BuildingRef
dbAddBuilding = dbAddObjectComposable BuildingRef dbPutBuilding buildingLocation

-- |
-- This deletes an object, which will cause future references to the same object
-- to fail.  Accepts a function to move all of the objects nested within the
-- object being deleted.
--
dbUnsafeDeleteObject :: (ReferenceType e) =>
        (forall m. DBReadable m =>
         Location (Reference ()) (Reference e) ->
         m (Location (Reference ()) ())) ->
    Reference e ->
    DB ()
dbUnsafeDeleteObject f ref =
    do _ <- dbMoveAllWithin f ref
       modify $ \db -> db {
           db_creatures = Map.delete (unsafeReference ref) $ db_creatures db,
           db_planes = Map.delete (unsafeReference ref) $ db_planes db,
           db_tools = Map.delete (unsafeReference ref) $ db_tools db,
           db_hierarchy = HD.delete (toUID ref) $ db_hierarchy db,
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
dbPutCreature = dbPutObjectComposable db_creatures (\x db_base_type ->
     db_base_type { db_creatures = x })

-- |
-- Puts a Plane under an arbitrary PlaneRef
--
dbPutPlane :: PlaneRef -> Plane -> DB ()
dbPutPlane = dbPutObjectComposable db_planes $
    \x db_base_type -> db_base_type { db_planes = x }

-- |
-- Puts a Tool under an arbitrary ToolRef
--
dbPutTool :: ToolRef -> Tool -> DB ()
dbPutTool = dbPutObjectComposable db_tools $
    \x db_base_type -> db_base_type { db_tools = x }

-- |
-- Puts a Building under an arbitrary BuildingRef
--
dbPutBuilding :: BuildingRef -> Building -> DB ()
dbPutBuilding = dbPutObjectComposable db_buildings $
    \x db_base_type -> db_base_type { db_buildings = x }

-- |
-- Gets an object from the database using getter functions.
--
dbGetObjectComposable :: (DBReadable db,Ord a,GenericReference a) => String -> (DB_BaseType -> Map a b) -> a -> db b
dbGetObjectComposable type_info get_fn ref = 
    asks (fromMaybe (error $ "dbGetObjectComposable: Nothing.  UID was " ++ show (toUID $ generalizeReference ref) ++ ", type info was " ++ type_info) . Map.lookup ref . get_fn)

-- |
-- Gets a Creature from a CreatureRef
--
dbGetCreature :: (DBReadable m) => CreatureRef -> m Creature
dbGetCreature = dbGetObjectComposable "CreatureRef" db_creatures

-- |
-- Gets a Plane from a PlaneRef
--
dbGetPlane :: (DBReadable m) => PlaneRef -> m Plane
dbGetPlane = dbGetObjectComposable "PlaneRef" db_planes

-- |
-- Gets a Plane from a PlaneRef
--
dbGetTool :: (DBReadable m) => ToolRef -> m Tool
dbGetTool = dbGetObjectComposable "ToolRef" db_tools

-- |
-- Gets a Plane from a PlaneRef
--
dbGetBuilding :: (DBReadable m) => BuildingRef -> m Building
dbGetBuilding = dbGetObjectComposable "BuildingRef" db_buildings

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
-- Modifies a Tool based on a PlaneRef.
--
dbModBuilding :: (Building -> Building) -> BuildingRef -> DB ()
dbModBuilding = dbModObjectComposable dbGetBuilding dbPutBuilding

-- |
-- Set the location of an object.
-- This is where we handle making sure that a creature can only wield one tool,
-- and a Plane can point to only one subsequent Plane.
--
dbSetLocation :: (LocationChild c,LocationParent p) => Location c p -> DB ()
dbSetLocation loc =
    do logDB log_database DEBUG $ "setting location: " ++ show loc
       case (fmap parent $ coerceParentTyped _wielded loc,
             fmap parent $ coerceParentTyped _subsequent loc,
             fmap parent $ coerceParentTyped _beneath loc) of
           (Just (Wielded c),_,_) -> dbUnwieldCreature c
           (_,Just (Subsequent s),_) -> shuntPlane _subsequent s
           (_,_,Just (Beneath b)) -> shuntPlane _beneath b
           (_,_,_) -> return ()
       modify (\db -> db { db_hierarchy = HD.insert (unsafeLocation loc) $ db_hierarchy db })

-- |
-- Shunt any wielded objects into inventory.
--
dbUnwieldCreature :: CreatureRef -> DB ()
dbUnwieldCreature c = mapM_ (dbSetLocation . returnToInventory) =<<
    dbGetContents c

-- |
-- Shunt a subordinate plane in the specified position to TheUniverse.
--
shuntPlane :: (LocationParent p) => Type p -> PlaneRef -> DB ()
shuntPlane t p = mapM_ (dbSetLocation . shuntToTheUniverse t) =<<
    dbGetContents p

-- |
-- Moves an object, returning the location of the object before and after
-- the move.
--
dbMove :: (ReferenceType e, LocationChild (Reference e),LocationParent b) =>
          (forall m. DBReadable m => Location (Reference e) () ->
                                     m (Location (Reference e) b)) ->
          (Reference e) ->
          DB (Location (Reference e) (),Location (Reference e) b)
dbMove moveF ref =
    do old <- dbWhere ref
       new <- ro $ moveF (unsafeLocation old)
       dbSetLocation (unsafeLocation new :: Location (Reference ()) ())
       when (genericParent old =/= genericParent new) $  -- an entity arriving in a new container shouldn't act before, nor be suspended beyond, the next action of the container
           dbSetTimeCoordinate ref =<< dbGetTimeCoordinate (genericParent new)
       return (unsafeLocation old, unsafeLocation new)

dbMoveAllWithin :: (forall m. DBReadable m => 
                       Location (Reference ()) (Reference e) ->
		       m (Location (Reference ()) ())) ->
                   Reference e ->
		   DB [(Location (Reference ()) (Reference e),Location (Reference ()) ())]
dbMoveAllWithin f ref = mapM (liftM (first unsafeLocation) . dbMove (f . unsafeLocation)) =<< dbGetContents ref

-- |
-- Verifies that a reference is in the database.
--
dbVerify :: (DBReadable db) => Reference e -> db Bool
dbVerify ref = asks (isJust . HD.parentOf (toUID ref) . db_hierarchy)

-- |
-- Returns the location of this object.
--
dbWhere :: (DBReadable db) => Reference e -> db (Location (Reference e) ())
dbWhere item = asks (unsafeLocation . fromMaybe (error "dbWhere: has no location") .
                       HD.lookupParent (toUID item) . db_hierarchy)

-- |
-- Returns all ancestor Locations of this element starting with the location
-- of the element and ending with theUniverse.
--
dbGetAncestors :: (DBReadable db,ReferenceType e) => Reference e -> db [Location (Reference ()) ()]
dbGetAncestors ref | isReferenceTyped _the_universe ref = return []
dbGetAncestors ref =
    do this <- dbWhere $ generalizeReference ref
       rest <- dbGetAncestors $ genericParent this
       return $ this : rest

-- |
-- Returns the location records of this object.
--
dbGetContents :: (DBReadable db,GenericReference a) => Reference t -> db [a]
dbGetContents item = asks (Data.Maybe.mapMaybe fromLocation . HD.lookupChildren 
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
dbAdvanceTime :: (ReferenceType a) => Reference a -> Rational -> DB ()
dbAdvanceTime ref t = dbSetTimeCoordinate ref =<< (return . (advanceTime t)) =<< dbGetTimeCoordinate ref

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
-- Answers the starting species.
--
dbGetStartingSpecies :: DB (Maybe Species)
dbGetStartingSpecies = do gets db_starting_species

-- |
-- Sets the starting species.
--
dbSetStartingSpecies :: Species -> DB ()
dbSetStartingSpecies the_species = modify (\db -> db { db_starting_species = Just the_species })

-- |
-- Takes a snapshot of a SnapshotEvent in progress.
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

