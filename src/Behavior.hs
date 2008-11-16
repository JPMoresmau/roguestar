module Behavior
    (Behavior(..),
     dbBehave)
    where

import DB
import DBData
import Facing
import Data.Ratio
import Tool
import Control.Monad.Error
import Combat
import Travel
import Creature
import Plane
import PlaneVisibility
import Data.List
import Control.Monad.Maybe

--
-- Every possible behavior that a creature might take, AI or Human.
--
data Behavior = 
    Step Facing
  | TurnInPlace Facing
  | Jump Facing
  | Pickup ToolRef
  | Wield ToolRef
  | Unwield
  | Drop ToolRef
  | Fire Facing
  | Attack Facing
  | Wait
  | Vanish

dbBehave :: Behavior -> CreatureRef -> DB ()
dbBehave (Step face) creature_ref =
    do dbMove (stepCreature face) creature_ref
       dbAdvanceTime (1%20) creature_ref

dbBehave (Jump face) creature_ref =
    do atomic $ liftM executeTeleportJump $ resolveTeleportJump creature_ref face
       dbAdvanceTime (2%20) creature_ref

dbBehave (TurnInPlace face) creature_ref =
    do dbMove (turnCreature face) creature_ref
       dbAdvanceTime (1%40) creature_ref

dbBehave (Pickup tool_ref) creature_ref =
    do dbMove (dbPickupTool creature_ref) tool_ref
       dbAdvanceTime (1%20) creature_ref

dbBehave (Wield tool_ref) creature_ref =
    do tool_parent <- liftM extractLocation $ dbWhere tool_ref
       when (tool_parent /= Just creature_ref) $ throwError $ DBErrorFlag "not-in-inventory"
       dbMove dbWieldTool tool_ref
       dbAdvanceTime (1%10) creature_ref

dbBehave (Unwield) creature_ref =
    do dbUnwieldCreature creature_ref
       dbAdvanceTime (1%40) creature_ref

dbBehave (Drop tool_ref) creature_ref =
    do tool_parent <- liftM extractLocation $ dbWhere tool_ref
       when (tool_parent /= Just creature_ref) $ throwError $ DBErrorFlag "not-in-inventory"
       dbMove dbDropTool tool_ref
       return ()

dbBehave (Fire face) creature_ref =
    do dbMove (turnCreature face) creature_ref
       atomic $ liftM dbExecuteRangedAttack $ dbResolveRangedAttack creature_ref face
       dbAdvanceTime (1%20) creature_ref
       return ()

dbBehave (Attack face) creature_ref =
    do dbMove (turnCreature face) creature_ref
       atomic $ liftM dbExecuteMeleeAttack $ dbResolveMeleeAttack creature_ref face
       dbAdvanceTime (1%20) creature_ref
       return ()

dbBehave Wait creature_ref =
    do dbAdvanceTime (1%40) creature_ref

dbBehave Vanish creature_ref = 
    do runMaybeT $
           do plane_ref <- MaybeT $ liftM (fmap $ fst . location) $ getPlanarLocation creature_ref
              lift $
                  do faction <- getCreatureFaction creature_ref
                     is_visible_to_anyone_else <- liftM (any (creature_ref `elem`)) $ 
	                 mapM (flip dbGetVisibleObjectsForFaction plane_ref) ({- all factions except this one: -} delete faction [minBound..maxBound])
                     when (not is_visible_to_anyone_else) $ deleteCreature creature_ref
       dbAdvanceTime (1%100) creature_ref
