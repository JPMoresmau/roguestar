module Models.EnergySwords
    (energySword) where

import RSAGL.Modeling
import RSAGL.Math
import Quality
import Models.LibraryData
import Control.Monad
import Models.Materials

energySword :: EnergyColor -> Integer -> Quality -> Modeling ()
energySword energy_color size_count _ = model $
    do model $ do closedCone (Point3D 0 (negate $ realToFrac size_count) 0,1.0) (Point3D 0 3 0,1.0)
                  concordance_metal
       model $ do closedDisc (Point3D 0 3.01 0) (Vector3D 0 1 0) 0.6
                  when (size_count >= 2) $ openDisc (Point3D 0 3.01 0) (Vector3D 0 1 0) 0.75 1.0
                  when (size_count >= 3) $ openCone (Point3D 0 2.75 0,1.1) (Point3D 0 2.25 0,1.1)
                  when (size_count >= 4) $ openCone (Point3D 0 (-2.25) 0,1.1) (Point3D 0 (-2.75) 0,1.1)
                  when (size_count >= 5) $ openCone (Point3D 0 (-4.25) 0,1.1) (Point3D 0 (-4.75) 0,1.1)
                  energyMaterial energy_color
