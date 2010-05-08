{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies, FlexibleContexts #-}

module AnimationTerrain
    (terrainThreadLauncher)
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

type TerrainThreadSwitch m = RSwitch Enabled (Maybe ProtocolTypes.TerrainTile) () () m

-- | Thread that launches rendering threads for the terrain tiles.
terrainThreadLauncher :: (FRPModel m) => FRP e (TerrainThreadSwitch m) () ()
terrainThreadLauncher = spawnThreads <<< arr (map (\x -> (Just x,terrainTile x))) <<< terrainElements

terrainTile :: (FRPModel m) => ProtocolTypes.TerrainTile -> FRP e (TerrainThreadSwitch m) () ()
terrainTile terrain_tile = proc () ->
    do t <- threadTime -< ()
       still_here <- renderTerrainTile terrain_tile -< fromSeconds $ min 0 $ toSeconds t - 1
       switchTerminate -< (if still_here then Nothing else Just $ terrainTile_Descending terrain_tile,())
       
terrainTile_Descending :: (FRPModel m) => ProtocolTypes.TerrainTile -> FRP e (TerrainThreadSwitch m) () ()
terrainTile_Descending terrain_tile = proc () ->
    do t <- arr ((subtract 1) . toSeconds) <<< threadTime -< ()
       killThreadIf -< t >= 1
       renderTerrainTile terrain_tile -< fromSeconds $ max 0 $ t
       returnA -< ()

renderTerrainTile :: (FRPModel m) => ProtocolTypes.TerrainTile -> FRP e (TerrainThreadSwitch m) Time Bool
renderTerrainTile (terrain_id@(ProtocolTypes.TerrainTile terrain_type (x,y))) = proc t ->
    do let awayness = max 0 $ min 0.99 $ (toSeconds t)^2
       terrain_elements <- terrainElements -< ()
       transformA libraryA -< (Affine $ translate (Vector3D (realToFrac x) 0 (negate $ realToFrac y)) . scale' (1 - awayness),
                               (scene_layer_local,Models.LibraryData.TerrainTile terrain_type))
       returnA -< isJust $ find (== terrain_id) terrain_elements

terrainElements :: (FRPModel m, StateOf m ~ AnimationState) => FRP e m () [ProtocolTypes.TerrainTile]
terrainElements = arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA <<< arr (const ("visible-terrain","0"))

