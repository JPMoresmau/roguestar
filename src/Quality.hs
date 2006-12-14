module Quality
    (Quality(..),
     qualitySor,
     qualityDeformedSor,
     qualityFrame,
     qualityTube)
    where

import Math3D
import Shapes
import Model
import Data.List

data Quality = Bad
	     | Poor
	     | Good
	     | Super
	     deriving (Eq,Enum,Ord,Show)

-- |
-- Tube (as Model.extrude using a circle as the extruded element) that
-- introduces both the number of subdivisions of latitude and longitude
-- according to the quality setting.
--
-- An assumption is that the extrusion is considerably longer than it is thick.
--
qualityTube :: Quality -> Texture -> [(Point3D,Float)] -> Model
qualityTube _ _ pts | newell (map fst pts) == Vector3D 0.0 0.0 0.0 = error "this extrusion is in a straight line, consider manually supplying a facing vector using Model.extrude or using a sor"
qualityTube Bad tex pts = extrude tex (genericLength pts + 1) (newell (map fst pts)) (reverse $ ring 5) pts
qualityTube Poor tex pts = extrude tex (genericLength pts + 1) (newell (map fst pts)) (reverse $ ring 8) pts
qualityTube Good tex pts = extrude tex (2 * genericLength pts + 1) (newell (map fst pts)) (reverse $ ring 16) pts
qualityTube Super tex pts = extrude tex (3 * genericLength pts + 1) (newell (map fst pts)) (reverse $ ring 36) pts

-- |
-- SOR (as Model.sor) that introduces the number
-- of subdivisions automatically according to the quality setting.
--
qualitySor :: Quality -> Texture -> [Point2D] -> Model
qualitySor q = qualityDeformedSor q id

qualityDeformedSor :: Quality -> (Point3D -> Point3D) -> Texture -> [Point2D] -> Model
qualityDeformedSor Bad dfn tex = deformedSor dfn tex 8
qualityDeformedSor Poor dfn tex = deformedSor dfn tex 16 . interpolateBetween2d
qualityDeformedSor Good dfn tex = deformedSor dfn tex 22 . interpolateBetween2d . interpolateBetween2d
qualityDeformedSor Super dfn tex = deformedSor dfn tex 44 . interpolateBetween2d . interpolateBetween2d . interpolateBetween2d

-- |
-- Generates a frame (as Model.frame), running interpolation
-- over the points to enhance the model according to the quality.
--
qualityFrame :: Quality -> Texture -> [[Point3D]] -> Model
qualityFrame Bad tex = frame tex
qualityFrame Poor tex = frame tex . enhancePoints
qualityFrame Good tex = frame tex . enhancePoints . enhancePoints
qualityFrame Super tex = frame tex . enhancePoints . enhancePoints . enhancePoints