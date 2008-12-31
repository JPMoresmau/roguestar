module Models.EnergyThings
    (energyCylinder) where

import Models.LibraryData
import RSAGL.Model
import RSAGL.ModelingExtras
import RSAGL.Vector
import Quality
import Data.Monoid
import Models.Materials

energyCylinder :: (Monoid attr) => EnergyColor -> Quality -> Modeling attr
energyCylinder c _ = model $
    do closedCone (Point3D 0 0 0,1.0) (Point3D 0 1 0,1.0)
       material $ do pigment $ pure blackbody
                     emissive $ pure $ energyColor c

