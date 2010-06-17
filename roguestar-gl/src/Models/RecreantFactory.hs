module Models.RecreantFactory
    (recreant_factory)
    where
    
import Quality
import RSAGL.Modeling
import RSAGL.Math
import Models.Materials

recreant_factory :: Quality -> Modeling ()
recreant_factory _ = model $
    do quadralateral (Point3D (-0.5) 0 (-0.5))
                     (Point3D (-0.5) 0 0.5)
		     (Point3D 0.5 0 0.5)
		     (Point3D 0.5 0 (-0.5))
       sphere (Point3D (-0.4) 0 (-0.4)) 0.1
       sphere (Point3D 0.4 0 (-0.4)) 0.1
       sphere (Point3D 0.4 0 0.4) 0.1
       sphere (Point3D (-0.4) 0 0.4) 0.1
       alliance_metal
