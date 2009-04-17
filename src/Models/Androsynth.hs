module Models.Androsynth
    (androsynth)
    where

import RSAGL.Math
import RSAGL.Modeling
import Quality
import Models.Materials

androsynth_head :: Quality -> Modeling ()
androsynth_head _ = model $
    do model $ 
           do smoothbox 0.2 (Point3D (-2) 0 (-2)) (Point3D (-3) 10 (-5))  -- side panels/"ears"
              smoothbox 0.2 (Point3D 2 0 (-2))    (Point3D 3 10 (-5))
              smoothbox 0.2 (Point3D (-5) 7 3)    (Point3D 5 8 (-8))         -- top panel
              smoothbox 0.2 (Point3D (-1) 2 2)    (Point3D 1 6 (-5))      -- main head
	      closedCone (Point3D (-3) 4 1,1) (Point3D (-3) 4 (-4),1)
	      closedCone (Point3D 3 4 1,1)    (Point3D 3 4 (-4),1)
	      model $
	          do quadralateral (Point3D 0 6 (-2))  (Point3D 0 10 (-2))
		                   (Point3D 0 10 (-5)) (Point3D 0 6 (-5))
		     twoSided True
	      openCone (Point3D 0 2 0,0.5) (Point3D 0 (-2) 0,0.5)
              concordance_metal
       let eyeglass = model $
            do sphere (Point3D 0 0 0) 0.75
	       affine $ scale (Vector3D 1 1 0.1)
               concordance_bright_glass
       model $ eyeglass >> affine (translate $ Vector3D 3 4 1)
       model $ eyeglass >> affine (translate $ Vector3D (-3) 4 1)
       model $
           do sphere (Point3D 0 4 2) 0.95
	      concordance_dark_glass

androsynth_body :: Quality -> Modeling ()
androsynth_body _ = model $
    do model $
           do smoothbox 0.2 (Point3D (-2) 7 (-2.5)) (Point3D 2 8 2.5)
	      smoothbox 0.2 (Point3D (-3) 0 (-3.5)) (Point3D 3 1 3.5)
	      openCone (Point3D 0 1 0,1.5) (Point3D 0 7 0,1)
       concordance_metal

androsynth :: Quality -> Modeling ()
androsynth q = model $ 
    do model $
           do androsynth_head q
              affine $ translate (Vector3D 0 30 0)
       model $
           do androsynth_body q
	      affine $ translate (Vector3D 0 20 0)
       affine $ scale' (1/40)
