{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
module Plane
    (dbNewPlane,
     planetName,
     randomPlanetName,
     planeDepth,
     dbGetCurrentPlane,
     dbDistanceBetweenSquared,
     pickRandomClearSite_withTimeout,
     pickRandomClearSite,
     getPlanarPosition,
     getBeneath,
     getSubsequent,
     terrainAt,
     setTerrainAt,
     whatIsOccupying,
     isTerrainPassable,
     getBiome)
    where

import Grids
import DB
import TerrainData
import PlaneData
import Control.Monad
import Data.Maybe
import Data.List
import Position
import PlayerState
import FactionData
import qualified Data.ByteString.Char8 as B
import Logging

dbNewPlane :: (PlaneLocation l) => B.ByteString -> TerrainGenerationData -> l -> DB PlaneRef
dbNewPlane name tg_data l =
    do rns <- getRandoms
       random_id <- getRandomR (1,1000000)
       dbAddPlane (Plane { plane_biome = tg_biome tg_data,
                           plane_terrain = generateTerrain tg_data rns,
                           plane_random_id = random_id,
                           plane_planet_name = name}) l

planetName :: (DBReadable db) => PlaneRef -> db B.ByteString
planetName = liftM plane_planet_name . dbGetPlane

randomPlanetName :: (DBReadable db) => Faction -> db B.ByteString
randomPlanetName faction =
    do planet_number <- getRandomR (1000 :: Integer,9999)
       return $ factionPrefix faction `B.append` "-" `B.append` B.pack (show planet_number)

planeDepth :: (DBReadable db) => PlaneRef -> db Integer
planeDepth this_plane =
    do l <- dbWhere this_plane
       case extractParent l of
           Just (Beneath above) -> liftM succ $ planeDepth above
           Nothing -> return 0

-- |
-- If this object is anywhere on a plane (such as carried by a creature who is on the plane),
-- returns the position of this object on that plane.
-- TODO: this function seems generic enough that it should be moved and renamed.
--
getPlanarPosition :: (DBReadable db,ReferenceType a,LocationParent p) =>
                     Reference a -> db (Maybe (Location (Reference ()) p))
getPlanarPosition ref =
    liftM (listToMaybe . mapMaybe coerceLocationRecord) $ dbGetAncestors ref

-- |
-- Get the plane beneath this one, if it exists.
--
getBeneath :: (DBReadable db) => PlaneRef -> db (Maybe PlaneRef)
getBeneath item =
    do (plane_locs :: [Location PlaneRef Beneath]) <- dbGetContents item
       return $ fmap child $ listToMaybe plane_locs

-- |
-- Get the plane subsequent to this one, if it exists.
--
getSubsequent :: (DBReadable db) => PlaneRef -> db (Maybe PlaneRef)
getSubsequent item =
    do (plane_locs :: [Location PlaneRef Subsequent]) <- dbGetContents item
       return $ fmap child $ listToMaybe plane_locs

-- |
-- Distance between two entities.  If the entities are not on the same plane, or for some other reason it doesn't make
-- sense to ask their distance, the Nothing.
--
dbDistanceBetweenSquared :: (DBReadable db,ReferenceType a,ReferenceType b) => Reference a -> Reference b -> db (Maybe Integer)
dbDistanceBetweenSquared a_ref b_ref =
    do m_a <- liftM (fmap parent) $ getPlanarPosition a_ref
       m_b <- liftM (fmap parent) $ getPlanarPosition b_ref
       return $
           do (p_a :: PlaneRef,a :: MultiPosition) <- m_a
              (p_b,b :: MultiPosition) <- m_b
              guard $ p_a == p_b
              return $ distanceBetweenSquared a b

-- |
-- Gets the current plane of interest based on whose turn it is.
--
dbGetCurrentPlane :: (DBReadable db) => db (Maybe PlaneRef)
dbGetCurrentPlane = liftM (fmap parent) $ maybe (return Nothing) getPlanarPosition . creatureOf =<< playerState

-- |
-- Selects sites at random until one seems reasonably clear.  It begins at
-- the specified Position on the map, and then picks more sites further and further away from the center
-- until it one seems clear -- this tends to concentrate sites near the center.
--
-- A site is considered clear if there are no objects at all within object_clear squares, and
-- only appropriate terrain (as defined by a predicate) within terrain_clear squares.
-- Distance is chessboard distance.
--
-- This function will gradually expand the search radius if encounters the slightest
-- difficulty finding a qualifying position.  The search radius parameter is strictly advisory.
--
-- This function can take an optional timeout parameter (pickRandomClearSite_withTimeout).  When used
-- without a timeout parameter, it may not terminate.  The only possible cause of non-termination is that no
-- site satisfies the terrain predicate.  However, if satisfactory sites are sufficiently rare,
-- extreme slowness is likely.
--
-- The timeout value should be a small integer greater or equal to one, since this function becomes slow with large timeout values.
--
pickRandomClearSite :: (DBReadable db) =>
    Integer -> Integer -> Integer ->
    Position -> (TerrainPatch -> Bool) -> PlaneRef ->
    db Position
pickRandomClearSite search_radius
                    object_clear
                    terrain_clear
                    p
                    terrainPredicate
                    plane_ref =
    liftM (fromMaybe $ error "pickRandomClearSite: impossible") $
        pickRandomClearSite_withTimeout Nothing
                                        search_radius
                                        object_clear
                                        terrain_clear
                                        p
                                        terrainPredicate
                                        plane_ref

pickRandomClearSite_withTimeout :: (DBReadable db) =>
    Maybe Integer -> Integer -> Integer -> Integer ->
    Position -> (TerrainPatch -> Bool) -> PlaneRef ->
    db (Maybe Position)
pickRandomClearSite_withTimeout (Just x) _ _ _ _ _ _ | x <= 0 = return Nothing
pickRandomClearSite_withTimeout timeout search_radius object_clear terrain_clear (Position (start_x,start_y)) terrainPredicate plane_ref =
    do logDB log_plane DEBUG $ "Searching for clear site . . ."
       xys <- liftM2 (\a b -> map Position $ zip a b)
           (mapM (\x -> liftM (+start_x) $ getRandomR (-x,x)) [1..search_radius])
           (mapM (\x -> liftM (+start_y) $ getRandomR (-x,x)) [1..search_radius])
       terrain <- liftM plane_terrain $ dbGetPlane plane_ref
       clutter_locations <- liftM (map (parent .
           asLocationTyped _nullary _multiposition)) $ dbGetContents plane_ref
       let terrainIsClear (Position (x,y)) =
               all terrainPredicate $
                   concat [[gridAt terrain (x',y') |
                            x' <- [x-terrain_clear..x+terrain_clear]] |
                            y' <- [y-terrain_clear..y+terrain_clear]]
       let clutterIsClear here = not $ any (\p -> distanceBetweenChessboard here p <= object_clear) clutter_locations
       let m_result = find (\p -> terrainIsClear p && clutterIsClear p) xys
       case m_result of
           Just result ->
               do logDB log_plane DEBUG "Found clear site."
                  return $ Just result
           Nothing -> pickRandomClearSite_withTimeout
                          (fmap (subtract 1) timeout)
                          (search_radius + 1)
                          object_clear
                          (max 0 $ terrain_clear - 1)
                          (Position (start_x,start_y))
                          terrainPredicate
                          plane_ref

terrainAt :: (DBReadable db) => PlaneRef -> Position -> db TerrainPatch
terrainAt plane_ref (Position (x,y)) =
    do terrain <- liftM plane_terrain $ dbGetPlane plane_ref
       return $ gridAt terrain (x,y)

setTerrainAt :: PlaneRef -> Position -> TerrainPatch -> DB ()
setTerrainAt plane_ref (Position pos) patch = dbModPlane (\p -> p { plane_terrain = specificReplaceGrid pos patch $ plane_terrain p }) plane_ref

-- | Lists all of the entities that are on a specific spot, not including nested entities.
-- Typically this is zero or one creatures, and zero or more tools.
whatIsOccupying :: (DBReadable db,GenericReference a) => PlaneRef -> Position -> db [a]
whatIsOccupying plane_ref position =
    liftM (mapMaybe fromLocation . filter ((== 0) .
                  (distanceBetweenChessboard position) . parent) .
              map (asLocationTyped _nullary _multiposition)) $
                  dbGetContents plane_ref

-- | Answers True iff a creature may walk or swim or drop objects at the position.  
-- Lava is considered passable, but trees are not.
isTerrainPassable :: (DBReadable db) => PlaneRef -> CreatureRef -> Position -> db Bool
isTerrainPassable plane_ref creature_ref position = 
    do (critters :: [Either BuildingRef CreatureRef]) <- liftM (filter (=/= creature_ref)) $ whatIsOccupying plane_ref position
       terrain <- terrainAt plane_ref position
       return $ not (terrain `elem` [RockFace,Forest,DeepForest]) && null critters

getBiome :: (DBReadable db) => PlaneRef -> db Biome
getBiome = liftM plane_biome . dbGetPlane
