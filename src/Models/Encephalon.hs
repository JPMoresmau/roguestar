module Models.Encephalon
    (encephalon)
    where
    
import Quality
import RSAGL.Vector
import RSAGL.CurveExtras
import RSAGL.Model
import RSAGL.ModelingExtras
import Models.EncephalonMaterials
import Models.AllianceMaterials
import RSAGL.Affine

encephalon_head :: Quality -> Modeling ()
encephalon_head _ = model $
    do sor $ linearInterpolation $
           points2d $ reverse
	            [(0.001,9),
                     (0.5,9),
                     (1,9),
                     (1.5,9),
                     (2,9),
                     (3,8.5),
                     (4,7),
                     (4,5),
                     (3,3)]
       deform dfn
       encephalon_skin
  where dfn (Point3D x y z) | y > (abs x ** 0.4) + 7.5 = Point3D x ((abs x ** 0.4) + 7.5) z 
        dfn pt = pt

encephalon_eye :: Quality -> Modeling ()
encephalon_eye _ = model $
    do sphere origin_point_3d 0.4
       pigment $ pure black

encephalon_suit :: Quality -> Modeling ()
encephalon_suit _ = model $ 
    do sor $ linearInterpolation $ 
           points2d $ reverse 
	            [(3,5),
                     (5,5),
                     (6,3),
                     (8,1),
                     (8,0.5),
		     (7,0),
		     (0.001,0)]
       alliance_metal
                          
encephalon :: Quality -> Modeling ()
encephalon q = model $ scale' (1/20) $
    do encephalon_head q
       encephalon_suit q
       translate (Vector3D (-1) 6 4) $ encephalon_eye q
       translate (Vector3D 1 6 4) $ encephalon_eye q
