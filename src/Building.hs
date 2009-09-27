module Building
    (buildingSize,
     buildingType)
    where

import DB
import BuildingData
import Data.List
import Control.Monad

-- | The total occupied surface area of a building.
buildingSize :: (DBReadable db) => BuildingRef -> db Integer
buildingSize = liftM (genericLength . buildingOccupies) . buildingType

buildingType :: (DBReadable db) => BuildingRef -> db BuildingType
buildingType building_ref =
    do constructed <- liftM extractLocation $ dbWhere building_ref
       case constructed of
           Just (Constructed _ _ building_type) -> return building_type
           _ -> error "buildingSize: impossible case"


