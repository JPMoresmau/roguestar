module Models.Recreant
    (recreant)
    where
    
import Math3D
import Model
import Quality

recreant_material :: Texture
recreant_material = SolidTexture $ rgbShine 1.0 (0.2,0.2,0.2)

recreant_antenna_small :: Quality -> Model
recreant_antenna_small q = qualitySor q recreant_material $
                           points2d [(1,6.5),
                                     (1.5,7),
                                     (2,6.5),
                                     (1.5,6)]
                                     
recreant_antenna_large :: Quality -> Model
recreant_antenna_large q = qualitySor q recreant_material $
                           points2d [(2,4.5),
                                     (2.5,5),
                                     (3,4.5),
                                     (2.5,4)]
                                     
recreant_body :: Quality -> Model
recreant_body q = qualitySor q recreant_material $
                  points2d [(0,3),
                            (4,3),
                            (6,2),
                            (7,1)]
                            
recreant :: Quality -> Model
recreant q = scaleModel 0.4 $
             Union [recreant_antenna_small q,
                    recreant_antenna_large q,
                    recreant_body q]