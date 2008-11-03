{-# LANGUAGE Arrows #-}

module Starships
    (StarshipInput(..),
     StarshipOutput(..),
     cyborgType4)
    where

import Animation
import Models.LibraryData
import RSAGL.CoordinateSystems
import RSAGL.Affine
import RSAGL.Vector
import Scene
import Control.Arrow
import RSAGL.LightSource
import RSAGL.Color
import RSAGL.Time
import RSAGL.FRP
import RSAGL.Angle
import RSAGL.AbstractVector

-- Unused
data StarshipInput = StarshipInput
-- Unused
data StarshipOutput = StarshipOutput

cyborgType4 :: RSAnimAX k t i o StarshipInput StarshipOutput
cyborgType4 = proc _ ->
    do transformA libraryA -< (Affine $ translate $ Vector3D 0 5 0,
           (scene_layer_orbit,CyborgType4Dome))
       libraryA -< (scene_layer_orbit,CyborgType4Base)
       hyperspace_fluctuation <- (arr $ sin . toSeconds) <<< threadTime -< ()
       transformA libraryA -< (Affine $ translate (Vector3D 0 (1 + hyperspace_fluctuation) 0) . scale (Vector3D 1 0.1 1),
           (scene_layer_orbit,CyborgType4HyperspaceDisc))
       transformA libraryA -< (Affine $ translate (Vector3D 0 (3 - hyperspace_fluctuation) 0) . scale (Vector3D 1 0.1 1),
           (scene_layer_orbit,CyborgType4HyperspaceDisc))
       rotor_rotation <- (arr $ fromDegrees . toSeconds . (`cyclical` fromSeconds 460)) <<< threadTime -< ()
       transformA libraryA -< (Affine $ rotate (Vector3D 0 1 0) (scalarMultiply 4 rotor_rotation),
           (scene_layer_orbit,CyborgType4HyperspaceRotor))
       transformA libraryA -< (Affine $ rotate (Vector3D 0 1 0) (fromDegrees 90) . rotate (Vector3D 0 1 0) (scalarMultiply 4 rotor_rotation),
           (scene_layer_orbit,CyborgType4HyperspaceRotor))
       transformA libraryA -< (Affine $ rotate (Vector3D 0 1 0) (fromDegrees 180) . rotate (Vector3D 0 1 0) (scalarMultiply 4 rotor_rotation),
           (scene_layer_orbit,CyborgType4HyperspaceRotor))
       transformA libraryA -< (Affine $ rotate (Vector3D 0 1 0) (fromDegrees 270) . rotate (Vector3D 0 1 0) (scalarMultiply 4 rotor_rotation),
           (scene_layer_orbit,CyborgType4HyperspaceRotor))
       transformA libraryA -< (Affine $ rotate (Vector3D 0 1 0) (scalarMultiply 3 rotor_rotation),
           (scene_layer_orbit,CyborgType4HyperspaceStabilizer))
       transformA libraryA -< (Affine $ rotate (Vector3D 0 1 0) (fromDegrees 120) . rotate (Vector3D 0 1 0) (scalarMultiply 3 rotor_rotation),
           (scene_layer_orbit,CyborgType4HyperspaceStabilizer))
       transformA libraryA -< (Affine $ rotate (Vector3D 0 1 0) (fromDegrees 240) . rotate (Vector3D 0 1 0) (scalarMultiply 3 rotor_rotation),
           (scene_layer_orbit,CyborgType4HyperspaceStabilizer))
       accumulateSceneA -< (scene_layer_orbit,lightSource $ PointLight {
           lightsource_position = Point3D 0 1.5 0,
           lightsource_radius = measure (Point3D 0 1.5 0) (Point3D 0 10 0),
           lightsource_color = rgb 0 0.33 0,
           lightsource_ambient = rgb 0 0 0 })
       returnA -< StarshipOutput
