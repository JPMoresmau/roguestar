-- | All construction (terrain clearing, etc) actions that a creature might take.
module Construction
    (modifyFacingTerrain,
     clearTerrain)
    where

import DB
import Plane
import TerrainData
import Facing
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Position
import Data.Maybe

-- | Modifies terrain in the specified walking direction, returning
-- True iff any terrain modification actually occured.
modifyFacingTerrain :: (TerrainPatch -> TerrainPatch) -> Facing -> CreatureRef -> DB Bool
modifyFacingTerrain f face creature_ref = liftM (fromMaybe False) $ runMaybeT $
    do (plane_ref,position) <- MaybeT $ liftM extractLocation $ dbWhere creature_ref
       let target_position = offsetPosition (facingToRelative face) position
       prev_terrain <- lift $ terrainAt plane_ref target_position
       let new_terrain = f prev_terrain
       when (new_terrain == prev_terrain) $ fail ""
       lift $ setTerrainAt plane_ref target_position new_terrain
       check_terrain <- lift $ terrainAt plane_ref target_position
       return True

clearTerrain :: TerrainPatch -> TerrainPatch
clearTerrain RockFace = Rubble
clearTerrain Forest = Grass
clearTerrain DeepForest = Grass
clearTerrain Lava = Glass
clearTerrain x = x
