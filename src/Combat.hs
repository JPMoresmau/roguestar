{-# LANGUAGE PatternGuards #-}

module Combat
    (dbResolveRangedAttack)
    where

import DB
import DBData
import Creature
import CreatureData
import Tool
import Control.Monad.Error
import Facing
import Data.Maybe
import Plane
import Dice
import Data.List
import Data.Ord
import Position

dbResolveRangedAttack :: CreatureRef -> Facing -> DB ()
dbResolveRangedAttack creature_ref face =
    do m_target <- liftM (listToMaybe . mapMaybe (toCreatureRef . entity)) $ dbFindRangedTargets creature_ref face
       tool_ref <- maybe (throwError $ DBErrorFlag "no-weapon-wielded") return =<< dbGetWielded creature_ref
       case m_target of
           Nothing -> dbPushSnapshot (DBMissEvent creature_ref tool_ref)
	   Just target_ref ->
	       do (attack_roll,defense_roll) <- dbGetOpposedRangedAttackRoll creature_ref target_ref
	          case () of
		      () | attack_roll > defense_roll -> dbPushSnapshot (DBAttackEvent creature_ref tool_ref target_ref)
		      () | otherwise -> dbPushSnapshot (DBMissEvent creature_ref tool_ref)

dbGetOpposedRangedAttackRoll :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db (Integer,Integer)
dbGetOpposedRangedAttackRoll attacker_ref defender_ref =
    do d <- liftM (fromMaybe (error "dbGetOpposedAttackRoll: defender and attacker are on different planes")) $ dbDistanceBetweenSquared attacker_ref defender_ref 
       attack_score <- dbGetCreatureScore RangedAttack attacker_ref 
       defense_score <- liftM (+ d) $ dbGetRangedDefenseScore defender_ref
       attack_roll <- roll [0..attack_score]
       defense_roll <- roll [0..defense_score]
       return (attack_roll,defense_roll)

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
       
