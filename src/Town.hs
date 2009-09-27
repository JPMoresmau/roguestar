module Town
    (createTown)
    where

import BuildingData
import DB
import DBData
import TerrainData
import Plane

-- | Create a town from a list of buildings.
createTown :: PlaneRef -> [BuildingType] -> DB [BuildingRef]
createTown plane_ref = mapM $ \building_type ->
    do let clear_need = minimum $ map abs $ uncurry (++) $ unzip $ buildingOccupies building_type
       p <- pickRandomClearSite 25 (clear_need*2+1) (clear_need+1) (Position (0,0)) (not . (`elem` difficult_terrains)) plane_ref
       dbAddBuilding Building $ Constructed {
           constructed_plane = plane_ref,
           constructed_position = p,
           constructed_type = building_type }
