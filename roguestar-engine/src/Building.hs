{-# LANGUAGE ScopedTypeVariables #-}

module Building
    (buildingSize,
     buildingType,
     activateFacingBuilding)
    where

import DB
import BuildingData
import Data.List
import Facing
import Data.Maybe
import Control.Monad.Maybe
import Plane
import Position
import TerrainData
import Control.Monad.Error
import CreatureData

-- | The total occupied surface area of a building.
buildingSize :: (DBReadable db) => BuildingRef -> db Integer
buildingSize = liftM (genericLength . buildingOccupies) . buildingType

buildingType :: (DBReadable db) => BuildingRef -> db BuildingType
buildingType building_ref =
    do constructed <- liftM extractParent $ dbWhere building_ref
       case constructed of
           Just (Constructed _ _ building_type) -> return building_type
           _ -> error "buildingSize: impossible case"

deleteBuilding :: BuildingRef -> DB ()
deleteBuilding = dbUnsafeDeleteObject (error "deleteBuilding: impossible case, buildings shouldn't contain anything.")

-- | Activate the facing building, returns True iff any building was actually activated.
activateFacingBuilding :: Facing -> CreatureRef -> DB Bool
activateFacingBuilding face creature_ref = liftM (fromMaybe False) $ runMaybeT $
    do (plane_ref,position) <- MaybeT $ liftM extractParent $ dbWhere creature_ref
       buildings <- lift $ whatIsOccupying plane_ref $ offsetPosition (facingToRelative face) position
       liftM or $ lift $ forM buildings $ \building_ref ->
           do building_type <- buildingType building_ref
              activateBuilding building_type creature_ref building_ref

activateBuilding :: BuildingType -> CreatureRef -> BuildingRef -> DB Bool
activateBuilding (Node _) creature_ref building_ref =
    do dbModCreature (\c -> c { creature_points = succ $ creature_points c }) creature_ref
       deleteBuilding building_ref
       return True
activateBuilding Portal creature_ref building_ref =
    do m_creature_position :: Maybe (PlaneRef,Position) <- liftM extractParent $ dbWhere creature_ref
       m_portal_position :: Maybe (PlaneRef,Position) <- liftM extractParent $ dbWhere building_ref
       when (fmap fst m_creature_position /= fmap fst m_portal_position) $ throwError $ DBError "activateBuilding: creature and portal on different planes"
       case (m_creature_position,m_portal_position) of
           (Just (plane_ref,Position (_,cy)),Just (_,Position (_,py))) ->
               case () of
                   () | cy < py ->
                       do m_subsequent_loc :: Maybe (Location PlaneRef Subsequent) <- liftM listToMaybe $ dbGetContents plane_ref
                          case m_subsequent_loc of
                              Just loc -> (portalCreatureTo 1 creature_ref $ child loc) >> return True
                              _ -> throwError $ DBErrorFlag NoStargateAddress
                   () | cy > py ->
                       do m_previous_loc :: Maybe Subsequent <- liftM extractParent $ dbWhere plane_ref
                          case m_previous_loc of
                              Just loc -> (portalCreatureTo (-1) creature_ref $ subsequent_to loc) >> return True
                              _ -> throwError $ DBErrorFlag NoStargateAddress
                   () | otherwise -> throwError $ DBErrorFlag BuildingApproachWrongAngle
           _ -> throwError $ DBError "activateBuilding: can't decode building-creature relative positions"


-- | Deposit a creature in front of (-1) or behind (+1) a random portal on the specified plane.  Returns
-- the dbMove result from the action.
portalCreatureTo :: Integer -> CreatureRef -> PlaneRef -> DB (Location CreatureRef (),Location CreatureRef Standing)
portalCreatureTo offset creature_ref plane_ref =
    do portals <- filterM (liftM (== Portal) . buildingType) =<< dbGetContents plane_ref
       ideal_position <- if null portals
           then liftM2 (\x y -> Position (x,y)) (getRandomR (-100,100)) (getRandomR (-100,100))
           else do portal <- pickM portals
                   m_position <- liftM (fmap (offsetPosition (0,offset)) . extractParent) $ dbWhere portal
                   return $ fromMaybe (Position (0,0)) m_position
       position <- pickRandomClearSite 1 0 0 ideal_position (not . (`elem` impassable_terrains)) plane_ref
       dbPushSnapshot $ TeleportEvent creature_ref
       dbMove (return . toStanding (Standing plane_ref position Here)) creature_ref

