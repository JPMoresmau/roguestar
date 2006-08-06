module AscensionClassStarship
    (ascension_class_starship)
    where

import ConcordanceStarshipParts
import Math3D
import Model

ascension_class_starship :: Model
ascension_class_starship = 
    Union [transformation (translationMatrix $ Vector3D 0 0 20) concordance_disk_section,
	   transformation (translationMatrix $ Vector3D (-10) 0 (-15)) concordance_engine_section,
	   transformation (translationMatrix $ Vector3D 10 0 (-15)) concordance_engine_section,
	   strip concordance_material_1 $
	   points3d_2 $ [((-7,-4,-14),(-2,-4,-20)),
			 ((-10,0,-15),(-10,0,-30))],
	   strip concordance_material_1 $
	   points3d_2 $ [((7,-4,-14),(2,-4,-20)),
			 ((10,0,-15),(10,0,-30))],
	   frame concordance_material_1 $
	   map points3d [[(0,0.01,21),  --bottom edge
			  (0,0.01,20),
			  (0,-1,10),
			  (0,-3,0),
			  (0,-6,-10),
			  (0,-11,-10),
			  (0,-10,-15),
			  (0,-9,-19),
			  (0,-8,-22),
			  (0,-7,-24),
			  (0,-6,-25),
			  (0,-5.51,-25)],
			 [(-0.02,0,21),
			  (-0.02,0,20),
			  (-2,-1,10),
			  (-3,-2,0),
			  (-3,-4,-10),
			  (-5,-4,-12),
			  (-6,-7,-14),
			  (-2,-5,-20),
			  (-1.5,-4.55,-22),
			  (-1,-4.25,-24),
			  (-1.01,-5,-25),
			  (-0.02,-5.5,-25)],
			 [(-0.01,0,21),  --right edge
			  (-0.01,0,20),
			  (-2,0,10),
			  (-3,-1,0),
			  (-3,-3,-10),
			  (-5,-3.5,-12),
			  (-7,-4,-14),
			  (-2,-4,-20),
			  (-1.5,-4,-22),
			  (-1,-4,-24),
			  (-1,-5,-25),
			  (-0.01,-5.5,-25)],
			 [(0,-0.01,9),  --top edge
			  (0,0,10),
			  (0,-1,0),
			  (0,-2,-10),
			  (0,-3,-12),
			  (0,-4,-14),
			  (0,-4,-16),
			  (0,-4,-18),
			  (0,-4,-20),
			  (0,-4,-24),
			  (0,-5,-25),
			  (0,-5.49,-25)],
			 [(0.01,0,21),  --left edge
			  (0.01,0,20),
			  (2,0,10),
			  (3,-1,0),
			  (3,-3,-10),
			  (5,-3.5,-12),
			  (7,-4,-14),
			  (2,-4,-20),
			  (1.5,-4,-22),
			  (1,-4,-24),
			  (1,-5,-25),
			  (0.01,-5.5,-25)],
			 [(0.02,0,21),
			  (0.02,0,20),
			  (2,-1,10),
			  (3,-2,0),
			  (3,-4,-10),
			  (5,-4,-12),
			  (6,-7,-14),
			  (2,-5,-20),
			  (1.5,-4.5,-22),
			  (1,-4.25,-24),
			  (1.01,-5,-25),
			  (0.02,-5.5,-25)]]
	  ] -- Union