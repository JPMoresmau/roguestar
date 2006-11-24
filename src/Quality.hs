module Quality
    (Quality(..),
     improve,
     reduce,
     qualitySOR,
     qualityFrame)
    where

import Model

data Quality = Bad
	     | Poor
	     | Good
	     | Super
	     deriving (Eq,Enum,Ord,Show)

-- |
-- Might improve the Quality.  Appropriate for a major piece
-- of geometry that needs extra detail.
--
improve :: Quality -> Quality
improve Bad = Bad
improve Poor = Good
improve Good = Good
improve Super = Super

-- |
-- Might reduce the quality.  Appropriate for a small piece of
-- geometry that doesn't need much detail.
--
reduce :: Quality -> Quality
reduce Bad = Bad
reduce Poor = Poor
reduce Good = Poor
reduce Super = Good

-- |
-- SOR (as Model.sor) that introduces the number
-- of subdivisions automatically according to the quality setting.
--
qualitySOR :: Quality -> Texture -> [Point2D] -> Model
qualitySOR Bad tex = sor tex 8
qualitySOR Poor tex = sor tex 16
qualitySOR Good tex = sor tex 22
qualitySOR Super tex = sor tex 28

-- |
-- Generates a frame (as Model.frame), running interpolation
-- over the points to enhance the model according to the quality.
--
qualityFrame :: Quality -> Texture -> [[Point3D]] -> Model
qualityFrame Bad tex = frame tex
qualityFrame Poor tex = frame tex . enhancePoints
qualityFrame Good tex = frame tex . enhancePoints . enhancePoints
qualityFrame Super tex = frame tex . enhancePoints . enhancePoints . enhancePoints