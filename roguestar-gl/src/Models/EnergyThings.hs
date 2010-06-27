module Models.EnergyThings
    (energyCylinder) where

import Models.LibraryData
import RSAGL.Modeling
import RSAGL.Math
import Quality
import Data.Monoid
import Models.Materials
import RSAGL.Color.RSAGLColors

energyCylinder :: (Monoid attr) => EnergyColor -> Quality -> Modeling attr
energyCylinder c _ = model $
    do closedCone (Point3D 0 0 0,1.0) (Point3D 0 1 0,1.0)
       material $ do pigment $ pure blackbody
                     emissive $ pure $ energyColor c

