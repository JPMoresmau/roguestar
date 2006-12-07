module Models.QuestionMark
    (question_mark)
    where
    
import Math3D
import Model
import Quality

question_mark_material :: Texture
question_mark_material = SolidTexture $ rgbLum 0.25 (1.0,0.5,1.0)

question_mark :: Quality -> Model
question_mark q =
    scale (Vector3D (-1) 1 1) $ 
    scaleModel 0.5 $ 
    Union [qualitySOR q question_mark_material
                        [Point2D 0.1 (-4.5),
                         Point2D 0.5 (-5),
                         Point2D 0.1 (-5.5)],
           qualityTube q question_mark_material
                         [(Point3D (-2) 0 0,0.5),
                          (Point3D (-2) 2 0,0.5),
                          (Point3D (-1) 3 0,0.5),
                          (Point3D 0 4 0,0.5),
                          (Point3D 2 4 0,0.5),
                          (Point3D 3 3 0,0.5),
                          (Point3D 3 0 0,0.5),
                          (Point3D 2 (-0.5) 0,0.5),
                          (Point3D 0 (-1) 0,0.5),
                          (Point3D 0 (-3) 0,0.5)]] 