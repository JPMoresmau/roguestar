module Models.Monolith
    (monolith)
    where

import RSAGL.Math
import RSAGL.Modeling
import Quality

monolith :: Quality -> Modeling ()
monolith _ =
    do box (Point3D (-1/2) 0 (-1/8)) (Point3D (1/2) (9/4) (1/8))
       material $
           do pigment $ pure blackbody
              specular 100 $ pure white 
