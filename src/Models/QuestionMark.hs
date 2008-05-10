module Models.QuestionMark
    (question_mark)
    where
    
import Quality
import RSAGL.Model
import RSAGL.ModelingExtras
import RSAGL.Affine
import RSAGL.Vector
import RSAGL.CurveExtras
import RSAGL.Curve

question_mark_material :: Modeling () 
question_mark_material = material $
    do pigment $ pure blackbody
       emissive $ pure fuchsia

question_mark :: Modeling ()
question_mark = model $ scale' 0.1 $ 
    do sor $ linearInterpolation $
           points2d [(0.1,-4.5),
                     (0.5,-5),
                     (0.1,-5.5)]
       tube $ zipCurve (,) (pure 0.5) $ linearInterpolation $
           points2d [(2,0),
                     (2,2),
                     (1,3),
                     (0,4),
                     (-2,4),
                     (-3,3),
                     (-3,0),
                     (-2,-0.5),
                     (0,-1),
                     (0,-3)]
       question_mark_material
