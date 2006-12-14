module Models.Encephalon
    (encephalon)
    where
    
import Math3D
import Model
import Quality

encephalon_material :: Texture
encephalon_material = proceduralTexture (synthesizePerlinNoise 1.61 (0.5,3))
                                        [(0.0, rgbShine 0.1 (0.6,1.0,0.6)),
                                         (1.0, rgbShine 0.1 (0.15,0.25,0.15))]

encephalon_suit_material :: Texture
encephalon_suit_material = SolidTexture $ rgbShine 1.0 (0.7,0.7,0.7)

encephalon_head :: Quality -> Model
encephalon_head q = qualityDeformedSor q dfn encephalon_material pts 
                    where pts = points2d [(0,9),
                                          (0.5,9),
                                          (1,9),
                                          (1.5,9),
                                          (2,9),
                                          (3,8.5),
                                          (4,7),
                                          (4,5),
                                          (3,3)]
                          dfn (Point3D x y z) | y > (abs x ** 0.4) + 7.5 = Point3D x ((abs x ** 0.4) + 7.5) z 
                          dfn pt = pt

encephalon_suit :: Quality -> Model
encephalon_suit q = qualitySor q encephalon_suit_material pts 
                    where pts = points2d [(3,5),
                                          (5,5),
                                          (6,3),
                                          (8,1),
                                          (10,0)]
                          
encephalon :: Quality -> Model
encephalon q = Union [encephalon_head q,
                      encephalon_suit q]