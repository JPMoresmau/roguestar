{-# LANGUAGE Arrows #-}

module AnimationTerrain
    (terrainThreadLauncher,
     terrainTileThreadIdentity)
    where

import Data.List
import RSAGL.FRP
import RSAGL.Math
import Animation
import Control.Arrow
import Data.Maybe
import Models.LibraryData
import ProtocolTypes
import Scene

-- | Thread that launches rendering threads for the terrain tiles.
terrainThreadLauncher :: RSAnimA (Maybe ProtocolTypes.TerrainTile) () () () ()
terrainThreadLauncher = spawnThreads <<< arr (map (\x -> (Just x,terrainTile x))) <<< terrainElements

terrainTile :: ProtocolTypes.TerrainTile -> RSAnimA (Maybe ProtocolTypes.TerrainTile) () () () ()
terrainTile terrain_tile = proc () ->
    do t <- threadTime -< ()
       still_here <- renderTerrainTile terrain_tile -< fromSeconds $ min 0 $ toSeconds t - 1
       switchTerminate -< (if still_here then Nothing else Just $ terrainTile_Descending terrain_tile,())
       
terrainTile_Descending :: ProtocolTypes.TerrainTile -> RSAnimA (Maybe ProtocolTypes.TerrainTile) () () () ()
terrainTile_Descending terrain_tile = proc () ->
    do t <- threadTime -< ()
       killThreadIf -< t >= fromSeconds 1
       renderTerrainTile terrain_tile -< t
       returnA -< ()

renderTerrainTile :: ProtocolTypes.TerrainTile -> RSAnimA t i o Time Bool
renderTerrainTile (ProtocolTypes.TerrainTile terrain_type (x,y)) = proc t ->
    do let awayness = max 0 $ min 0.99 $ (toSeconds t)^2
       terrain_elements <- terrainElements -< ()
       transformA libraryA -< (Affine $ translate (Vector3D (realToFrac x) 0 (negate $ realToFrac y)) . scale' (1 - awayness),
                               (scene_layer_local,Models.LibraryData.TerrainTile terrain_type))
       returnA -< isJust $ find (\a -> tt_xy a == (x,y)) terrain_elements

terrainElements :: RSAnimA t i o () [ProtocolTypes.TerrainTile]
terrainElements = arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA <<< arr (const ("visible-terrain","0"))

terrainTileThreadIdentity :: ThreadIdentity ProtocolTypes.TerrainTile
terrainTileThreadIdentity = unionThreadIdentity (\a b -> tt_xy a == tt_xy b)
