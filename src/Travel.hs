module Travel
    (stepCreature,
     jumpCreature,
     turnCreature)
    where

import Control.Monad.Maybe
import Facing
import DB
import Plane
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Data.Ord
import Position
import TerrainData
import Data.List (minimumBy)
import Creature
import CreatureData

walkCreature :: (DBReadable db) => Facing -> (Integer,Integer) -> 
                                     Location m CreatureRef () -> db (Location m CreatureRef ())
walkCreature face (x',y') l = liftM (fromMaybe l) $ runMaybeT $
    do (plane_ref,Position (x,y)) <- MaybeT $ return $ extractLocation l
       let standing = Standing { standing_plane = plane_ref,
                                 standing_position = Position (x+x',y+y'),
                                 standing_facing = face } 
       flip unless (fail "") =<< (lift $ isTerrainPassable plane_ref (entity l) $ standing_position standing)
       return $ generalizeLocation $ toStanding standing l

-- |
-- Try to teleport the creature to the specified Position.  The teleport attempt can be automatically retried a number of times, and the most accurate attempt will be used.
-- If the retries are negative, the teleport will be made artificially innacurate.
--
randomTeleportLanding :: (DBReadable db) => Integer -> PlaneRef -> Position -> Position -> db Position
randomTeleportLanding retries plane_ref source_destination goal_destination =
    do landings <- replicateM (fromInteger $ max 1 retries) (pickRandomClearSite (1 + max 0 (negate retries)) 0 0 goal_destination (not . (`elem` impassable_terrains)) plane_ref)
       return $ minimumBy (comparing $ \p -> distanceBetweenSquared goal_destination p * distanceBetweenSquared source_destination p) landings

-- |
-- Teleport jump a creature about 7 units in the specified direction.  Teleports can fail either by stalling (not teleporting) or by teleporting to a random incorrect location.
--
jumpCreature :: (DBReadable db) => Facing -> Location m CreatureRef () -> db (Location m CreatureRef ())
jumpCreature face start_location = liftM (fromMaybe start_location) $ runMaybeT $
    do jump_roll <- liftM roll_actual $ lift $ rollCreatureAbilityScore JumpSkill 0 (entity start_location)
       standing_location <- MaybeT $ return $ extractLocation start_location
       let jump_offset = facingToRelative7 face
       end_location <- lift $ walkCreature face jump_offset start_location
       case () of
           -- critical fail, don't jump:
           () | jump_roll <= 0 -> return start_location
           -- successful jump:
           () | start_location /= end_location -> return end_location
           -- jump into impassable terrain, guess a random landing site
           () | otherwise -> lift $
               do landing_position <- randomTeleportLanding (floor $ (+1) $ sqrt $ realToFrac jump_roll) (standing_plane standing_location) (standing_position standing_location) $
                      offsetPosition jump_offset $ standing_position standing_location
                  return $ generalizeLocation $ toStanding (standing_location { standing_position = landing_position }) start_location

stepCreature :: (DBReadable db) => Facing -> Location m CreatureRef () -> db (Location m CreatureRef ())
stepCreature face = walkCreature face (facingToRelative face)

turnCreature :: (DBReadable db) => Facing -> Location m CreatureRef () -> db (Location m CreatureRef ())
turnCreature face = walkCreature face (0,0)

