module ConcordanceStarshipParts
    (concordance_material_1,
     concordance_disk_section,
     concordance_engine_section)
    where

import Model
import Math3D

concordance_material_1 :: Color
concordance_material_1 = rgbColor (0.6,0.6,0.6)

concordance_disk_section :: Model
concordance_disk_section =
    sor concordance_material_1 22 $
	points2d [(0,3),
		  (5,3),
		  (15,2),
		  (17.5,1.1),
		  (20,0.1),
		  (20,0),
		  (15,0),
		  (5,-0.5),
		  (0,-0.5)]
	    

concordance_engine_section :: Model
concordance_engine_section =
    frame concordance_material_1 $ reverse $
    map points3d [[(-0.02,-0.02,5),
		   (-5,1,1),
		   (-5,1,-1),
		   (-2,2,-5),
		   (-1,1,-15),
		   (-0.12,0,-30)],
		  [(-0.01,-0.01,5),
		   (-5,0,1),
		   (-5,0,-1),
		   (-2,0,-5),
		   (-1,0,-15),
		   (-0.01,0,-30)],
		  [(0.01,-0.01,5),
		   (5,0,1),
		   (5,0,-1),
		   (2,0,-5),
		   (1,0,-15),
		   (0.01,0,-30)],
		  [(0.02,-0.02,5),
		   (5,1,1),
		   (5,1,-1),
		   (2,2,-5),
		   (1,1,-15),
		   (0.12,0,-30)],
		  [(0,0.01,5),  -- top edge
		   (0,1,0),
		   (0,2,0),
		   (0,3,-5),
		   (0,1,-15),
		   (0,0.01,-30)]]