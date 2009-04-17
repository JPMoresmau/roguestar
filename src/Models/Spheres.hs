module Models.Spheres
    (gasSphere,
     metalSphere,
     chromaliteSphere)
    where

import Quality
import RSAGL.Modeling
import RSAGL.Math
import Models.Materials
import Models.LibraryData

-- | An empty (or transparent) gas sphere.
gasSphere :: Quality -> Modeling ()
gasSphere _ = model $
    do sphere (Point3D 0 0.06 0) 0.06
       material $ 
           do transparent $ pure $ rgba 0.9 0.9 0.9 0.25
              specular 10 $ pure white

metalSphere :: Quality -> Modeling ()
metalSphere _ = model $
    do sphere (Point3D 0 0.05 0) 0.05
       concordance_metal

chromaliteSphere :: Quality -> Modeling ()
chromaliteSphere _ = model $
    do sphere (Point3D 0 0.04 0) 0.04
       energyMaterial Yellow
