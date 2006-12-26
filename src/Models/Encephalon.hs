module Models.Encephalon
    (encephalon,
     encephalon_scale_factor,
     encephalon_suit_material)
    where
    
import Math3D
import Model
import Quality

encephalon_scale_factor :: Vector3D
encephalon_scale_factor = scaleModelFactor 0.4 $ encephalon_suit Super

encephalon_material :: Texture
encephalon_material = proceduralTexture (synthesizePerlinNoise 1.61 (0.5,3))
                                        [(0.0, rgbShine 0.1 (0.6,1.0,0.6)),
                                         (1.0, rgbShine 0.1 (0.15,0.25,0.15))]

encephalon_eye_material :: Texture
encephalon_eye_material = SolidTexture $ rgbShine 1.0 (0.0,0.0,0.0)

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

encephalon_eye :: Quality -> Model
encephalon_eye q = qualitySor (min q Poor) encephalon_eye_material pts
                   where pts = points2d [(0,0.4),
                                         (0.2,0.3),
                                         (0.4,0),
                                         (0.2,-0.3),
                                         (0,-0.4)]

encephalon_suit :: Quality -> Model
encephalon_suit q = qualitySor q encephalon_suit_material pts 
                    where pts = points2d [(3,5),
                                          (5,5),
                                          (6,3),
                                          (8,1),
                                          (10,0)]
                          
encephalon :: Quality -> Model
encephalon q = scale encephalon_scale_factor $
               Union [encephalon_head q,
                      encephalon_suit q,
                      translate (Vector3D (-1) 6 4) $ encephalon_eye q,
                      translate (Vector3D 1 6 4) $ encephalon_eye q]