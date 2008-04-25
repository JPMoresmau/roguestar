module Models.RecreantFactory
    (recreant_factory)
    where
    
import Quality
import RSAGL.Model
import RSAGL.CurveExtras
import RSAGL.Material
import RSAGL.ModelingExtras
import RSAGL.Vector
import RSAGL.Affine
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
       recreant_metal
