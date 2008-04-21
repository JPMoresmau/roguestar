module Behavior
    (Behavior(..),
     dbBehave)
    where

import DB
import DBData
import Facing
import Creature
import Data.Ratio
import Tool
import Control.Monad.Error
import Combat

--
-- Every possible behavior that a creature might take, AI or Human.
--
data Behavior = 
    Step Facing
  | TurnInPlace Facing
  | Pickup ToolRef
  | Wield ToolRef
  | Unwield
  | Drop ToolRef
  | Fire Facing
  | Attack Facing
  | Wait

dbBehave :: Behavior -> CreatureRef -> DB ()
dbBehave (Step face) creature_ref =
    do dbStepCreature face creature_ref
       dbAdvanceTime (1%20) creature_ref

dbBehave (TurnInPlace face) creature_ref =
    do dbTurnCreature face creature_ref
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
    do dbTurnCreature face creature_ref
       atomic $ liftM dbExecuteRangedAttack $ dbResolveRangedAttack creature_ref face
       dbAdvanceTime (1%20) creature_ref
       return ()

dbBehave (Attack face) creature_ref =
    do dbTurnCreature face creature_ref
       atomic $ liftM dbExecuteMeleeAttack $ dbResolveMeleeAttack creature_ref face
       dbAdvanceTime (1%20) creature_ref
       return ()

dbBehave Wait creature_ref =
    do dbAdvanceTime (1%40) creature_ref
