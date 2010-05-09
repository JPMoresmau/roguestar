{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies, FlexibleContexts #-}

module AnimationTerrain
    (terrainThreadLauncher)
    where

import Data.List
import RSAGL.FRP
import RSAGL.Math
import RSAGL.Animation.InverseKinematics
import Animation
import Control.Arrow
import Data.Maybe
import Models.LibraryData
import ProtocolTypes
import Scene
import AnimationExtras

type TerrainThreadSwitch m = RSwitch Enabled (Maybe ProtocolTypes.TerrainTile) () () m

-- | Thread that launches rendering threads for the terrain tiles.
terrainThreadLauncher :: (FRPModel m) => FRP e (TerrainThreadSwitch m) () ()
terrainThreadLauncher = spawnThreads <<<
                        arr (map (\x -> (Just x,terrainTile x))) <<<
                        newListElements <<< terrainElements

terrainTile :: (FRPModel m) => ProtocolTypes.TerrainTile ->
                               FRP e (TerrainThreadSwitch m) () ()
terrainTile (tid@(ProtocolTypes.TerrainTile terrain_type (x,y))) = proc () ->
    do terrain_elements <- terrainElements -< ()
       let still_here = isJust $ find (== tid) terrain_elements
       let goal_size = if still_here then 1.25 else -0.25
       actual_size <- arr (max 0 . min 1) <<<
                      approachFrom 0.5 (perSecond 1.0) 0 -< goal_size
       killThreadIf -< actual_size <= 0.0 && not still_here
       transformA libraryA -<
           (Affine $ translate
                         (Vector3D (fromInteger x) 0 (negate $ fromInteger y)) .
                     scale' actual_size,
               (scene_layer_local,Models.LibraryData.TerrainTile terrain_type))
       returnA -< ()

terrainElements :: (FRPModel m, StateOf m ~ AnimationState) => FRP e m () [ProtocolTypes.TerrainTile]
terrainElements = arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA <<< arr (const ("visible-terrain","0"))

