module Models.Recreant
    (recreant)
    where
    
import Quality
import RSAGL.Modeling
import RSAGL.Math.CurveExtras
import RSAGL.Math
import Models.Materials

recreant_antenna_small :: Quality -> Modeling ()
recreant_antenna_small _ = 
    do sor $ linearInterpolation $
           points2d $ reverse
	            [(1,6.5),
                     (1.5,7),
                     (2,6.5),
                     (1.5,6)]
                                     
recreant_antenna_large :: Quality -> Modeling ()
recreant_antenna_large _ = 
    do sor $ linearInterpolation $
           points2d $ reverse 
	            [(2,4.5),
                     (2.5,5),
                     (3,4.5),
                     (2.5,4)]
                                     
recreant_body :: Quality -> Modeling ()
recreant_body _ = 
    do sor $ linearInterpolation $
                  points2d $ reverse
		           [(0,3),
                            (4,3),
                            (6,2),
                            (7,1),
			    (0,0)]
                            
recreant :: Quality -> Modeling ()
recreant q = scale' 0.05 $
    do recreant_antenna_small q
       recreant_antenna_large q
       recreant_body q
       alliance_metal
