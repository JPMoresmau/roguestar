module Planet
    (makePlanets,
     generatePlanetInfo)
    where

import PlanetData
import DB
import DBData
import Plane
import TerrainData
import Control.Monad
import Control.Monad.Random
import Data.Maybe
import Data.Ord
import Town
import Data.List

makePlanet :: (PlaneLocation l) => l -> PlanetInfo -> DB PlaneRef
makePlanet l planet_info =
    do seed <- getRandom
       plane_ref <- dbNewPlane (TerrainGenerationData {
           tg_smootheness = 3,
	   tg_biome = planet_info_biome planet_info,
	   tg_placements = [recreantFactories seed] }) l
       town <- liftM catMaybes $ forM (planet_info_town planet_info) $ \(r,b) ->
           do p <- rationalRoll r
              return $ if p then Just b else Nothing
       createTown plane_ref town
       return plane_ref

makePlanets :: (PlaneLocation l) => l -> [PlanetInfo]  -> DB PlaneRef
makePlanets _ [] = return $ error "makePlanetarySystem: empty list"
makePlanets l (planet_info:rest) =
    do plane_ref <- makePlanet l planet_info
       makePlanets (Subsequent plane_ref) rest
       return plane_ref

generatePlanetInfo :: (DBReadable db) => [PlanetInfo] -> db [PlanetInfo]
generatePlanetInfo planet_infos = liftM (sortBy (comparing planet_info_priority) . concat) $ forM planet_infos $ \planet_info ->
    do prio_bonus <- liftM (sum . take 4) $ getRandomRs (0.0,0.25) -- see documentation for 'PlanetData.PlanetInfo'
       return $ planet_info `addPriority` prio_bonus
       return [planet_info]

