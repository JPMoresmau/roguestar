{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}

module Behavior
    (Behavior(..),
     facingBehavior,
     dbBehave)
    where

import DB
import Position
import Facing
import Data.Ratio
import Tool
import Control.Monad.Error
import Combat
import Activate
import Travel
import TravelData
import Creature
import CreatureData
import Plane
import PlaneVisibility
import Data.List
import Control.Monad.Maybe
import TerrainData
import Make
import Construction
import Building

--
-- Every possible behavior that a creature might take, AI or Human.
--
data Behavior =
    Step Facing
  | TurnInPlace Facing
  | StepDown
  | StepUp
  | Jump Facing
  | Pickup ToolRef
  | Wield ToolRef
  | Unwield
  | Drop ToolRef
  | Fire Facing
  | Attack Facing
  | Wait
  | Vanish
  | Activate
  | Make PrepareMake
  | ClearTerrain Facing
  | ActivateBuilding Facing
        deriving (Show)

-- | Get an appropriate behavior facing in the given direction.
-- If the adjacent facing square is empty, this is 'Step', but
-- if occupied by a creature this is 'Attack'.
facingBehavior :: (DBReadable db) => CreatureRef -> Facing -> db Behavior
facingBehavior creature_ref face =
    do (m_standing :: Maybe (PlaneRef,Position)) <- liftM (fmap parent) $ getPlanarPosition creature_ref
       case m_standing of
           Nothing -> return Wait
           Just (plane_ref,pos) ->
               do let facing_pos = offsetPosition (facingToRelative face) pos
                  t <- terrainAt plane_ref facing_pos
                  who :: [CreatureRef] <- whatIsOccupying plane_ref facing_pos
                  what :: [BuildingRef] <- whatIsOccupying plane_ref facing_pos
                  case t of
                      _ | not (null who) -> return $ Attack face
                      _ | not (null what) -> return $ ActivateBuilding face
                      Forest -> return $ ClearTerrain face
                      DeepForest -> return $ ClearTerrain face
                      RockFace -> return $ ClearTerrain face
                      _ -> return $ Step face

dbBehave :: Behavior -> CreatureRef -> DB ()
dbBehave (Step face) creature_ref =
    do (move_from,move_to) <- dbMove (stepCreature face) creature_ref
       dbAdvanceTime creature_ref =<< case () of
           () | (move_from == move_to) -> return 0
           () | face == Here -> quickActionTime creature_ref -- counts as turning in place
           () | face `elem` [North,South,East,West] -> move1ActionTime creature_ref
           () | otherwise -> move2ActionTime creature_ref

dbBehave StepDown creature_ref =
    do _ <- atomic executeClimb $ resolveClimb creature_ref ClimbDown
       -- FIXME: should be conditional
       dbAdvanceTime creature_ref =<< fullActionTime creature_ref

dbBehave StepUp creature_ref =
    do _ <- atomic executeClimb $ resolveClimb creature_ref ClimbUp
       -- FIXME: should be conditional
       dbAdvanceTime creature_ref =<< fullActionTime creature_ref

dbBehave (Jump face) creature_ref =
    do _ <- atomic executeTeleportJump $ resolveTeleportJump creature_ref face
       dbAdvanceTime creature_ref =<< fullActionTime creature_ref

dbBehave (TurnInPlace face) creature_ref =
    do _ <- dbMove (turnCreature face) creature_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave (Pickup tool_ref) creature_ref =
    do _ <- dbMove (dbPickupTool creature_ref) tool_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave (Wield tool_ref) creature_ref =
    do available <- availableWields creature_ref
       already_wielded <- dbGetWielded creature_ref
       when (not $ tool_ref `elem` available) $ throwError $ DBErrorFlag ToolIs_Unreachable
       _ <- dbMove dbWieldTool tool_ref
       dbAdvanceTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0 -- already wielded, so this was an empty action
           () | otherwise -> quickActionTime creature_ref

dbBehave (Unwield) creature_ref =
    do dbUnwieldCreature creature_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave (Drop tool_ref) creature_ref =
    do tool_parent <- liftM extractParent $ dbWhere tool_ref
       already_wielded <- dbGetWielded creature_ref
       when (tool_parent /= Just creature_ref) $ throwError $ DBErrorFlag ToolIs_NotInInventory
       _ <- dbMove dbDropTool tool_ref
       dbAdvanceTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0  -- instantly drop a tool if it's already held in the hand
           () | otherwise -> quickActionTime creature_ref

dbBehave (Fire face) creature_ref =
    do _ <- dbMove (turnCreature face) creature_ref
       ranged_attack_model <- rangedAttackModel creature_ref
       _ <- atomic executeAttack $ resolveAttack ranged_attack_model face
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       return ()

dbBehave (Attack face) creature_ref =
    do _ <- dbMove (turnCreature face) creature_ref
       melee_attack_model <- meleeAttackModel creature_ref
       _ <- atomic executeAttack $ resolveAttack melee_attack_model face
       dbAdvanceTime creature_ref =<< move1ActionTime creature_ref
       return ()

dbBehave Wait creature_ref = dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave Vanish creature_ref = 
    do dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       _ <- runMaybeT $
           do (plane_ref :: PlaneRef) <- MaybeT $ liftM (fmap parent) $ getPlanarPosition creature_ref
              lift $
                  do faction <- getCreatureFaction creature_ref
                     is_visible_to_anyone_else <- liftM (any (creature_ref `elem`)) $ 
	                 mapM (\fact -> dbGetVisibleObjectsForFaction (return . const True) fact plane_ref) 
                             ({- all factions except this one: -} delete faction [minBound..maxBound])
                     when (not is_visible_to_anyone_else) $ deleteCreature creature_ref
       return ()

dbBehave Activate creature_ref =
    do _ <- atomic executeActivation $ resolveActivation creature_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       return ()

dbBehave (Make make_prep) creature_ref =
    do _ <- atomic executeMake $ resolveMake creature_ref make_prep
       dbAdvanceTime creature_ref =<< fullActionTime creature_ref
       return ()

dbBehave (ClearTerrain face) creature_ref =
    do _ <- dbMove (turnCreature face) creature_ref
       ok <- modifyFacingTerrain clearTerrain face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       dbAdvanceTime creature_ref =<< fullActionTime creature_ref
       return ()

dbBehave (ActivateBuilding face) creature_ref =
    do _ <- dbMove (turnCreature face) creature_ref
       ok <- activateFacingBuilding face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       dbAdvanceTime creature_ref =<< fullActionTime creature_ref

{---------------------------------------------------------------------------------------------------
-- These are functions related to determing how long it takes for a creature to execute an action.
----------------------------------------------------------------------------------------------------}

-- | A value indicating the degree of difficulty a creature suffers on account of the inventory it is carrying.
inventoryBurden :: (DBReadable db) => CreatureRef -> db Rational
inventoryBurden creature_ref =
    do inventory_size <- liftM (genericLength . map (asType _tool)) $ dbGetContents creature_ref
       inventory_skill <- liftM roll_ideal $ rollCreatureAbilityScore InventorySkill 0 creature_ref
       return $ (inventory_size ^ 2) % inventory_skill

-- | Multiplier penalty if a creature is overweighted.
overweightPenalty :: (DBReadable db) => CreatureRef -> db Rational
overweightPenalty = liftM (max 1.0) . inventoryBurden

-- | Multiplier penalty if a creature is injured.
healthPenalty :: (DBReadable db) => CreatureRef -> db Rational
healthPenalty creature_ref =
    do current_health <- getCreatureHealth creature_ref
       raw_speed <- liftM (rawScore Speed) $ dbGetCreature creature_ref
       return $ (max 1.0 $ recip $ max (1%raw_speed) current_health) -- maximum health penalty determined by speed

-- | Multiplier penalties for doing anything that requires physical movement, e.g. walking.
physicalActionPenalties :: (DBReadable db) => CreatureRef -> db Rational
physicalActionPenalties creature_ref =  liftM2 (*) (overweightPenalty creature_ref) (healthPenalty creature_ref)

-- | Time required to do a simple physical task.
quickActionTime :: (DBReadable db) => CreatureRef -> db Rational
quickActionTime creature_ref = liftM2 (*) (physicalActionPenalties creature_ref) (liftM ((3%) . rawScore Speed) $ dbGetCreature creature_ref)

-- | Time required to move one step.
move1ActionTime :: (DBReadable db) => CreatureRef -> db Rational
move1ActionTime creature_ref = liftM2 (*) (physicalActionPenalties creature_ref) (liftM ((5%) . rawScore Speed) $ dbGetCreature creature_ref)

-- | Time required to move diagonally one step.
move2ActionTime :: (DBReadable db) => CreatureRef -> db Rational
move2ActionTime = liftM (*1.4142) . move1ActionTime

-- | Time required to complete a complex physical action.
fullActionTime :: (DBReadable db) => CreatureRef -> db Rational
fullActionTime = liftM (*2) . move1ActionTime
