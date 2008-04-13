{-# LANGUAGE PatternGuards #-}

module Combat
    (dbResolveRangedAttack,
     dbExecuteRangedAttack)
    where

import DB
import DBData
import Creature
import CreatureData
import Tool
import ToolData
import Control.Monad.Error
import Facing
import Data.Maybe
import Plane
import Dice
import Data.List
import Data.Ord
import Position

data RangedAttackOutcome =
    RangedAttackMiss CreatureRef ToolRef
  | RangedAttackHitCreature CreatureRef ToolRef CreatureRef Integer

dbResolveRangedAttack :: (DBReadable db) => CreatureRef -> Facing -> db RangedAttackOutcome
dbResolveRangedAttack attacker_ref face =
    do m_defender_ref <- liftM (listToMaybe . mapMaybe (toCreatureRef . entity)) $ dbFindRangedTargets attacker_ref face
       tool_ref <- maybe (throwError $ DBErrorFlag "no-weapon-wielded") return =<< dbGetWielded attacker_ref
       attack_roll <- dbRollRangedAttack attacker_ref
       damage_roll <- dbRollRangedDamage attacker_ref tool_ref
       case m_defender_ref of
           Nothing -> return $ RangedAttackMiss attacker_ref tool_ref
           Just defender_ref ->
	       do defense_roll <- dbRollRangedDefense attacker_ref defender_ref
                  injury_roll <- dbRollInjury defender_ref damage_roll
		  return $ case () of
		      () | attack_roll > defense_roll -> RangedAttackHitCreature attacker_ref tool_ref defender_ref injury_roll
		      () | otherwise -> RangedAttackMiss attacker_ref tool_ref

dbExecuteRangedAttack :: RangedAttackOutcome -> DB ()
dbExecuteRangedAttack (RangedAttackMiss attacker_ref tool_ref) = 
    do dbPushSnapshot (DBMissEvent attacker_ref tool_ref)
dbExecuteRangedAttack (RangedAttackHitCreature attacker_ref tool_ref defender_ref damage) =
    do dbPushSnapshot (DBAttackEvent attacker_ref tool_ref defender_ref)
       dbInjureCreature damage defender_ref
       dbSweepDead =<< liftM genericParent (dbWhere attacker_ref)

dbRollRangedDamage :: (DBReadable db) => CreatureRef -> ToolRef -> db Integer
dbRollRangedDamage _ weapon_ref =
    do tool <- dbGetTool weapon_ref
       case tool of
           GunTool g ->
	       do energy_released <- roll [0..gunEnergyOutput g]
	          energy_throughput <- roll [0..gunThroughput g] -- todo: overheats if energy_released > energy_throughput
		  return $ min energy_released energy_throughput

dbRollRangedAttack :: (DBReadable db) => CreatureRef -> db Integer
dbRollRangedAttack attacker_ref =
    do attack_score <- dbGetCreatureScore RangedAttack attacker_ref
       roll [0..attack_score]

dbRollRangedDefense :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Integer
dbRollRangedDefense attacker_ref defender_ref =
    do d <- liftM (fromMaybe (error "dbGetOpposedAttackRoll: defender and attacker are on different planes")) $ dbDistanceBetweenSquared attacker_ref defender_ref 
       defense_score <- liftM (+ d) $ dbGetRangedDefenseScore defender_ref
       defense_roll <- roll [0..defense_score]
       return defense_roll

dbGetRangedDefenseScore :: (DBReadable db) => Reference a -> db Integer
dbGetRangedDefenseScore ref | Just creature_ref <- toCreatureRef ref = dbGetCreatureScore RangedDefense creature_ref
dbGetRangedDefenseScore _ = return 10

dbFindRangedTargets :: (DBReadable db,ReferenceType a) => Reference a -> Facing -> db [Location S (Reference ()) Position]
dbFindRangedTargets attacker_ref face =
    do m_l <- dbGetPlanarLocation attacker_ref
       flip (maybe $ return []) m_l $ \(plane_ref,pos) ->
           liftM (sortBy (comparing (distanceBetweenSquared pos . location)) .
	          filter ((/= genericReference attacker_ref) . entity) . 
	          filter (isFacing (pos,face) . location) . 
		  mapMaybe coerceLocation) $ 
		      dbGetContents plane_ref
       
