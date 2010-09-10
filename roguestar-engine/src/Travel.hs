{-# LANGUAGE ScopedTypeVariables #-}

module Travel
    (stepCreature,
     turnCreature,
     stepDown,
     stepUp,
     TeleportJumpOutcome,
     resolveTeleportJump,
     executeTeleportJump)
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
import Logging

walkCreature :: (DBReadable db) => Facing -> (Integer,Integer) ->
                                     Location CreatureRef () -> db (Location CreatureRef ())
walkCreature face (x',y') l = liftM (fromMaybe l) $ runMaybeT $
    do (plane_ref,Position (x,y)) <- MaybeT $ return $ extractParent l
       let standing = Standing { standing_plane = plane_ref,
                                 standing_position = Position (x+x',y+y'),
                                 standing_facing = face }
       flip unless (fail "") =<< (lift $ isTerrainPassable plane_ref (child l) $ standing_position standing)
       return $ generalizeParent $ toStanding standing l

stepCreature :: (DBReadable db) => Facing -> Location CreatureRef () -> db (Location CreatureRef ())
stepCreature face = walkCreature face (facingToRelative face)

turnCreature :: (DBReadable db) => Facing -> Location CreatureRef () -> db (Location CreatureRef ())
turnCreature face = walkCreature face (0,0)

--------------------------------------------------------------------------------
--      Travel between planes.
--------------------------------------------------------------------------------

stepDown :: (DBReadable db) => Location CreatureRef () ->
                           db (Location CreatureRef ())
stepDown l =
    do m_new_location <- runMaybeT $
           do ((p,pos) :: (PlaneRef,Position)) <- MaybeT $ return $ extractParent l
              lift $ logDB log_travel DEBUG $ "Stepping down from: " ++ show (p,pos)
              let face = fromMaybe Here $ extractParent l
              p' <- MaybeT $ getBeneath p
              lift $ logDB log_travel DEBUG $ "Stepping down to: " ++ show p'
              pos' <- lift $ pickRandomClearSite 10 0 0 pos (== Upstairs) p'
              return $ generalizeParent $ toStanding
                  (Standing { standing_plane = p',
                              standing_position = pos',
                              standing_facing = face }) l
       case m_new_location of
           Just l' -> return l'
           Nothing ->
               do logDB log_travel WARNING "stepDown: couldn't find destination"
                  return l

stepUp :: (DBReadable db) => Location CreatureRef () ->
                             db (Location CreatureRef ())
stepUp l =
    do m_new_location <- runMaybeT $
           do ((p,pos) :: (PlaneRef,Position)) <- MaybeT $ return $ extractParent l
              let face = fromMaybe Here $ extractParent l
              (p' :: PlaneRef) <- MaybeT $ liftM extractParent $ dbWhere p
              pos' <- lift $ pickRandomClearSite 10 0 0 pos (== Downstairs) p'
              return $ generalizeParent $ toStanding
                  (Standing { standing_plane = p',
                              standing_position = pos',
                              standing_facing = face }) l
       case m_new_location of
           Just l' -> return l'
           Nothing ->
               do logDB log_travel WARNING "stepUp: couldn't find destination"
                  return l

--------------------------------------------------------------------------------
--      Teleportation/Jumping
--------------------------------------------------------------------------------

-- |
-- Try to teleport the creature to the specified Position.  The teleport attempt can be automatically retried a number of times, and the most accurate attempt will be used.
-- If the retries are negative, the teleport will be made artificially innacurate.
--
randomTeleportLanding :: (DBReadable db) => Integer -> PlaneRef -> Position -> Position -> db Position
randomTeleportLanding retries plane_ref source_destination goal_destination =
    do landings <- replicateM (fromInteger $ max 1 retries) $ (pickRandomClearSite 3) 0 0 goal_destination (not . (`elem` impassable_terrains)) plane_ref
       return $ minimumBy (comparing $ \p -> distanceBetweenSquared goal_destination p ^ 2 * distanceBetweenSquared source_destination p) landings

data TeleportJumpOutcome =
    TeleportJumpGood CreatureRef Standing
  | TeleportJumpFailed

-- |
-- Teleport jump a creature about 7 units in the specified direction.
--
resolveTeleportJump :: (DBReadable db) => CreatureRef -> Facing -> db TeleportJumpOutcome
resolveTeleportJump creature_ref face = liftM (fromMaybe TeleportJumpFailed) $ runMaybeT $
    do start_location <- lift $ dbWhere creature_ref
       jump_roll <- liftM roll_log $ lift $ rollCreatureAbilityScore JumpSkill 0 (child start_location)
       standing_location <- MaybeT $ return $ extractParent start_location
       landing_position <- lift $ randomTeleportLanding jump_roll (standing_plane standing_location) (standing_position standing_location) $
           offsetPosition (facingToRelative7 face) $ standing_position standing_location
       case () of
           () | jump_roll <= 0 -> return TeleportJumpFailed
           () | otherwise -> return $ TeleportJumpGood (child start_location) $ standing_location { standing_position = landing_position, standing_facing = face }

-- | Execute a resolved teleport jump.
executeTeleportJump :: TeleportJumpOutcome -> DB ()
executeTeleportJump TeleportJumpFailed = return ()
executeTeleportJump (TeleportJumpGood creature_ref standing_location) = 
    do _ <- dbMove (return . toStanding standing_location) creature_ref
       dbPushSnapshot $ TeleportEvent creature_ref
       return ()

