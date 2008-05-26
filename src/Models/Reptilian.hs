module Models.Reptilian
    (reptilian,
     reptilian_leg_upper,
     reptilian_leg_lower,
     reptilian_arm_upper,
     reptilian_arm_lower)
    where

import RSAGL.Vector
import RSAGL.Model
import Quality
import Models.Materials
import RSAGL.ModelingExtras
import RSAGL.CurveExtras
import RSAGL.Affine
import RSAGL.Angle

reptilian :: Quality -> Modeling ()
reptilian _ = model $
    do model $
           do tube $ linearInterpolation
                  [(0    ,Point3D 0 0   (-6)),
		   (1    ,Point3D 0 5   (-4)),
		   (1.5  ,Point3D 0 5.5 (-3)),
		   (2.4  ,Point3D 0 6   (-2)),
		   (2    ,Point3D 0 7   (-1.5)),
		   (1.2    ,Point3D 0 8   (-1)),
		   (0.5  ,Point3D 0 9   (-1.5)),
		   (1.5  ,Point3D 0 10.5 (-1)),
		   (1    ,Point3D 0 11  0),
		   (1    ,Point3D 0 11  1),
		   (1    ,Point3D 0 11  2),
		   (0.5  ,Point3D 0 10  2.5),
		   (0    ,Point3D 0 9   3)]
              model $
	          do triangle (Point3D 0 7 (-2)) (Point3D (-1) 10 (-8)) (Point3D 1 10 (-8))
		     triangle (Point3D 0 7 (-2)) (Point3D (-0.25) 9 (-9)) (Point3D (-3.5) 9.5 (-8))
		     triangle (Point3D 0 7 (-2)) (Point3D 0.25 9 (-9)) (Point3D 3.5 9.5 (-8))
		     triangle (Point3D 0 7 (-2)) (Point3D (-0.5) 8 (-8.5)) (Point3D (-5.5) 9 (-7))
		     triangle (Point3D 0 7 (-2)) (Point3D 0.5 8 (-8.5)) (Point3D 5.5 9 (-7))
                     twoSided True
		     deform $ \(Point3D x y z) -> Point3D x (y+x^2/100) (z+x^2/25+sin (x*10)/4)
	      reptilian_skin 
       model $
           do sphere (Point3D 0.55 11 2) 0.5
	      sphere (Point3D (-0.55) 11 2) 0.5
	      material $ pigment $ pure black
       affine $ scale' (1/20)

reptilian_leg_upper :: Quality -> Modeling ()
reptilian_leg_upper _ = rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do model $ sor $ linearInterpolation $
           points2d [(0  ,0),
	             (1.9,1),
		     (1.6,5),
		     (1,7.5),
		     (0.75,10.6),
		     (0  ,10.8)]
       material $
           do pigment $ pattern (gradient origin_point_3d (Vector3D 0 10 0)) [(0.0,reptilian_pigment),(1.0,pure burgundy)]
              specular 5.0 $ pattern (gradient origin_point_3d (Vector3D 0 10 0)) [(0.0,reptilian_specular),(1.0,pure crimson)]
       affine $ scale' (1/10)

reptilian_leg_lower :: Quality -> Modeling ()
reptilian_leg_lower _ = rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do sor $ linearInterpolation $
           points2d [(0  ,-0.15),
	             (0.5,-0.1),
	             (1.0,0.0),
		     (0.5,9.0),
		     (0.5,10.0),
		     (0  ,10.1)]
       openCone (Point3D 0 9.5 0,0.5) (Point3D 0 10 7,0.0001)
       openCone (Point3D 0 9.5 0,0.5) (Point3D 5 10 5,0.0001)
       openCone (Point3D 0 9.5 0,0.5) (Point3D (-5) 10 5,0.0001)
       material $ 
          do pigment $ pure burgundy
             specular 5.0 $ pure crimson
       affine $ scale' (1/10)

reptilian_arm_upper :: Quality -> Modeling ()
reptilian_arm_upper _ = rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do sor $ linearInterpolation $
	   points2d [(0  ,0.0),
	             (1.0,0.1),
	             (0.5,1.0),
	   	     (0.25,5.0),
		     (0.5,9.0),
		     (1.0,9.9),
		     (0  ,0.0)]
       material $
           do pigment $ pure burgundy
              specular 5.0 $ pure crimson
       affine $ scale' (1/10)

reptilian_arm_lower :: Quality -> Modeling ()
reptilian_arm_lower _ = rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do sor $ linearInterpolation $
           points2d [(0  ,-1.0),
	             (1.0,0.0),
		     (0.25,5.0),
		     (0.5,9.0),
		     (0.5,9.9),
		     (0  ,0.0)]
       material $
           do pigment $ pure burgundy
              specular 5.0 $ pure crimson
       affine $ scale' (1/10)
       
