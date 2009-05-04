{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, Rank2Types #-}
module RSAGL.Math.Curve
    (Curve,
     zipCurve,
     iterateCurve,
     transposeCurve,
     curve,
     Surface,
     surface,
     wrapSurface,
     unwrapSurface,
     transposeSurface,
     zipSurface,
     iterateSurface,
     halfIterateSurface,
     flipTransposeSurface,
     translateCurve,
     scaleCurve,
     clampCurve,
     loopCurve,
     controlCurve,
     transformCurve2,
     uv_identity,
     translateSurface,
     scaleSurface,
     transformSurface,
     transformSurface2,
     surfaceDerivative,
     curveDerivative,
     orientationLoops,
     newellCurve,
     surfaceNormals3D,
     SamplingAlgorithm,
     IntervalSample,
     intervalRange,
     intervalSize,
     intervalSample,
     intervalSingleIntegral,
     linearSamples,
     adaptiveMagnitudeSamples,
     integrateCurve)
    where

import RSAGL.Math.Vector
import RSAGL.Math.Angle
import RSAGL.Auxiliary.Auxiliary
import RSAGL.Math.Affine
import Data.List
import Data.Maybe
import Control.Parallel.Strategies
import Control.Applicative
import RSAGL.Math.AbstractVector
import Debug.Trace
import RSAGL.Modeling.BoundingBox
import RSAGL.Math.Interpolation
import RSAGL.Math.FMod

-- | A parametric function that is aware of it's own sampling interval.  The first parameter is the sampling interval, while the second is the curve input parameter.
type CurveF a = (Double,Double) -> a
-- | A surface is a curve of curves.
type SurfaceF a = CurveF (CurveF a)
-- | A 'Curve' is a parametric function from a one-dimensional space into a space of an arbitrary datatype.  The key feature of a 'Curve' is that it is aware of it's own
-- sampling interval.  Using this information and appropriate arithmetic and scalar multiplication functions provided by RSAGL.AbstractVector, a 'Curve' can be differentiated or integrated.
newtype Curve a = Curve { fromCurve :: CurveF a }

instance Functor Curve where
   fmap g (Curve f) = Curve $ g . f

instance Applicative Curve where
    pure a = Curve $ const a
    f <*> a = zipCurve ($) f a

instance (AffineTransformable a) => AffineTransformable (Curve a) where
    scale v = fmap (scale v)
    translate v = fmap (translate v)
    rotate vector angle = fmap (rotate vector angle)
    transform m = fmap (transform m)
    rotateX angle = fmap (rotateX angle)
    rotateY angle = fmap (rotateY angle)
    rotateZ angle = fmap (rotateZ angle)

instance NFData (Curve a) where
    rnf (Curve f) = seq f ()

-- | Sample a specific point on a curve given the sampling interval (first parameter) and the point on the curve (second parameter).
sampleCurve :: Curve a -> Double -> Double -> a
sampleCurve (Curve f) = curry f

-- | Sample a curve at regular intervals in the range 0..1 inclusive.
iterateCurve :: Integer -> Curve x -> [x]
iterateCurve n c = map f $ zeroToOne n
    where f = sampleCurve c (0.25/fromInteger n)

-- | Combine two curves using an arbitrary function.
zipCurve :: (x -> y -> z) -> Curve x -> Curve y -> Curve z
zipCurve f (Curve x) (Curve y) = Curve $ \hu -> f (x hu) (y hu)

-- | Arbitrarily transform a 'Curve'.
mapCurve :: (CurveF a -> CurveF a) -> Curve a -> Curve a
mapCurve f = Curve . f . fromCurve

-- | Arbitrarily transform a surface 'Curve'.
mapCurve2 :: (SurfaceF a -> SurfaceF a) -> Curve (Curve a) -> Curve (Curve a)
mapCurve2 f = Curve . (Curve .) . f . (fromCurve .) . fromCurve

-- | Translate a curve along the axis of the input parameter.
translateCurve :: Double -> Curve a -> Curve a
translateCurve x = mapCurve $ \f (h,u) -> f (h,u-x)

-- | Scale a curve along the axis of the input parameter.  Factors greater than one have a "zoom in" effect, while
-- factors less than one have a "zoom out" effect.
scaleCurve :: Double -> Curve a -> Curve a
scaleCurve s = mapCurve (\f (h,u) -> f (h/s,u/s))

-- | Clamp lower and upper bounds of a curve along the axis of the input parameter.
clampCurve :: (Double,Double) -> Curve a -> Curve a
clampCurve (a,b) | b < a = clampCurve (b,a)
clampCurve (a,b) = mapCurve $ \f (h,u) -> f (h,min b $ max a u)

-- | Loop a curve onto itself at the specified bounds.
loopCurve :: (Double,Double) -> Curve a -> Curve a
loopCurve (a,b) | b < a = loopCurve (b,a)
loopCurve (a,b) = mapCurve $ \f (h,u) -> f (h,(u-a) `fmod` (b-a) + a)

-- | Transform a curve by manipulating control points.
controlCurve :: (Double,Double) -> (Double,Double) -> Curve a -> Curve a
controlCurve (u,v) (u',v') = translateCurve u' . scaleCurve ((u'-v') / (u-v)) . translateCurve (negate u)

-- | Transpose the inner and outer components of a curve.
transposeCurve :: Curve (Curve a) -> Curve (Curve a)
transposeCurve = mapCurve2 flip

-- | Lift two curve transformations onto each axis of a second order curve.
transformCurve2 :: (forall u. Curve u -> Curve u) -> (forall v. Curve v -> Curve v) -> Curve (Curve a) -> Curve (Curve a)
transformCurve2 fu fv = transposeCurve . fu . transposeCurve . fv

-- | Define a simple curve.
curve :: (Double -> a) -> Curve a
curve = Curve . uncurry . const

-- | A 'Surface' is a based on a 'Curve' with an output of another 'Curve'.
newtype Surface a = Surface (Curve (Curve a)) deriving (NFData,AffineTransformable)

-- | Define a simple surface.
surface :: (Double -> Double -> a) -> Surface a
surface f = Surface $ curve (\u -> curve $ flip f u)

wrapSurface :: Curve (Curve a) -> Surface a
wrapSurface = Surface

unwrapSurface :: Surface a -> Curve (Curve a)
unwrapSurface (Surface s) = s

-- | Transpose the axes of a 'Surface'.
transposeSurface :: Surface a -> Surface a
transposeSurface (Surface s) = Surface $ transposeCurve s

-- | Sample a surface at regularly spaced lattice points in the range 0..1 inclusive.
iterateSurface :: (Integer,Integer) -> Surface a -> [[a]]
iterateSurface (u,v) (Surface s) = map (iterateCurve u) $ iterateCurve v s

-- | Sample the outer 'Curve' of a 'Surface' at regularly spaced intervals.
halfIterateSurface :: Integer -> Surface a -> [Curve a]
halfIterateSurface u = iterateCurve u . unwrapSurface

instance Functor Surface where
    fmap f (Surface x) = Surface $ fmap (fmap f) x

instance Applicative Surface where
    pure a = surface (const $ const a)
    f <*> a = zipSurface ($) f a

-- | Combine two surfaces using an arbitrary function.
zipSurface :: (x -> y -> z) -> Surface x -> Surface y -> Surface z
zipSurface f (Surface x) (Surface y) = Surface $ zipCurve (zipCurve f) x y

-- | Lift a transformation on a second order 'Curve' onto a Surface.
transformSurface :: (Curve (Curve a) -> Curve (Curve a)) -> Surface a -> Surface a
transformSurface f = Surface . f . unwrapSurface

-- | Lift two curve transformations onto each axis of a Surface.
transformSurface2 :: (forall u. Curve u -> Curve u) -> (forall v. Curve v -> Curve v) -> Surface a -> Surface a
transformSurface2 fu fv = transformSurface (transformCurve2 fu fv)

-- | Translate a surface over each of its input parameter axes, as translateCurve.
translateSurface :: (Double,Double) -> Surface a -> Surface a
translateSurface (u,v) = transformSurface2 (translateCurve u) (translateCurve v)

-- | Scale a surface along each of its input parameter axes, as scaleCurve.
scaleSurface :: (Double,Double) -> Surface a -> Surface a
scaleSurface (u,v) = transformSurface2 (scaleCurve u) (scaleCurve v)

-- | Transpose a surface while flipping the inner curve, so that that orientable surfaces retain their original orientation.
flipTransposeSurface :: Surface a -> Surface a
flipTransposeSurface = transformSurface (mapCurve2 $ \f (hu,u) (hv,v) -> f (hu,u) (hv,1-v)) . transposeSurface

-- | An identity 'Surface'.
uv_identity :: Surface (Double,Double)
uv_identity = surface (curry id)

-- | Take the derivative of a 'Curve'.
curveDerivative :: (AbstractSubtract p v,AbstractScale v) => Curve p -> Curve v
curveDerivative (Curve f) = Curve $ \(h,u) -> scalarMultiply (recip $ 2 * h) $ f (h/2,u+h) `sub` f (h/2,u-h)

-- | Take the piecewise derivative of a 'Surface' along the inner and outer curves.
surfaceDerivative :: (AbstractSubtract p v,AbstractScale v) => Surface p -> Surface (v,v)
surfaceDerivative s = zipSurface (,) (curvewiseDerivative s) (transposeSurface $ curvewiseDerivative $ transposeSurface s)
    where curvewiseDerivative (Surface t) = Surface $ fmap curveDerivative t

-- | Determine the orientation of a 'Surface' by passing very small circles centered on each sampled point as the parametric input.
--
-- A gotchya with this operation is that as much as 3/4ths of the orientation loop may lie outside of the 0..1 range that is normally
-- sampled.  Depending on how the surface is constructed, this may produce unexpected results.  The solution is to clamp the
-- the problematic parametric inputs at 0 and 1 using 'clampSurface'.
--
-- As a rule, do clamp longitudinal axes that come to a singularity at each end.
-- Do not clamp latitudinal axes that are connected at each end.
--
orientationLoops :: Surface p -> Surface (Curve p)
orientationLoops (Surface s) = Surface $ Curve $ \(uh,u) -> Curve $ \(vh,v) ->
                                     curve $ \t -> f (uh/2,u + uh*(sine $ fromRotations t)) 
				                     (vh/2,v + vh*(cosine $ fromRotations t))
   where f = fromCurve . fromCurve s

-- | Try to determine the normal vector to a curve.
newellCurve :: Curve Point3D -> Maybe Vector3D
newellCurve c = newell $ iterateCurve 16 c

-- | Try to determine the normal vectors of a surface using orientation loops.  This is usually slower but more successful than 'surfaceNormals3DByPartialDerivatives'.
-- This generate a warning message if it cannot determine the normal vector at a sampled point.
-- See also 'orientationLoops'.
surfaceNormals3DByOrientationLoops :: Surface Point3D -> Surface SurfaceVertex3D
surfaceNormals3DByOrientationLoops s = SurfaceVertex3D <$> s <*> ((\c -> errmsg c (newellCurve c)) <$> orientationLoops s)
    where errmsg c = fromMaybe (trace ("surfaceNormals3DByOrientationLoops: zero normal gave up: " ++ show (iterateCurve 16 c)) (Vector3D 0 0 0))

-- | Try to determine the normal vectors of a surface using partial derivatives.
surfaceNormals3DByPartialDerivatives :: Surface Point3D -> Surface (Maybe Vector3D)
surfaceNormals3DByPartialDerivatives s = safeCrossProduct <$> surfaceDerivative s
    where x = snd $ boundingCenterRadius $ boundingBox $ concat $ iterateSurface (8,8) s
          safeCrossProduct (u_,v_) =
              do u <- aLargeVector (x/100) u_
	         v <- aLargeVector (x/100) v_
		 return $ vectorNormalize $ crossProduct u v

-- | Try to determine the normal vectors of a surface using multiple techniques.
surfaceNormals3D :: Surface Point3D -> Surface SurfaceVertex3D
surfaceNormals3D s = (\p by_pd by_newell -> case by_pd of
                           Just v -> SurfaceVertex3D p v
			   Nothing -> by_newell) <$>
                     s <*> (surfaceNormals3DByPartialDerivatives s) <*> (surfaceNormals3DByOrientationLoops s)

-- | An interval of a curve, including the curve, lower and upper bounds of the interval, and an instantaneous sample value for that interval.
data IntervalSample a = IntervalSample (Curve a) Double Double a

intervalSample :: Curve a -> Double -> Double -> IntervalSample a
intervalSample c l h = IntervalSample c l h $ sampleCurve c ((abs $ l - h) / 2) ((l+h) / 2) 

-- | Lower and upper bounds of an 'IntervalSample'.
intervalRange :: IntervalSample a -> (Double,Double)
intervalRange (IntervalSample _ l h _) = (l,h)

-- | Size of the range of an 'IntervalSample'.
intervalSize :: IntervalSample a -> Double
intervalSize (IntervalSample _ l h _) = abs $ h - l

-- | Instantaneous sample value of an 'Interval'.
intervalValue :: IntervalSample a -> a
intervalValue (IntervalSample _ _ _ a) = a

-- | Integral of the sample value over the range of the 'IntervalSample'.
intervalSingleIntegral :: (AbstractScale a) => IntervalSample a -> a
intervalSingleIntegral x = scalarMultiply (intervalSize x) $ intervalValue x

-- | Split an interval into three equal parts.
splitInterval :: IntervalSample a -> [IntervalSample a]
splitInterval (IntervalSample c l h a) = [intervalSample c l l',IntervalSample c l' h' a,intervalSample c h' h]
    where l' = lerp (1/3) (l,h)
          h' = lerp (2/3) (l,h)

type SamplingAlgorithm a = Curve a -> [IntervalSample a]

-- | Definite integral of a curve.
integrateCurve :: (AbstractAdd p v,AbstractScale v,AbstractZero p) => SamplingAlgorithm v -> Curve v -> p -> p
integrateCurve samplingAlgorithm c initial_value = foldl' add initial_value $ map intervalSingleIntegral $ samplingAlgorithm c

-- | Sampling algorithm that takes a fixed count of samples.
linearSamples :: Integer -> SamplingAlgorithm a
linearSamples n c = map (\(l,h) -> intervalSample c l h) $ doubles $ zeroToOne (n+1)

-- | Sampling algorithm that takes increasing numbers of samples over intervals where the magnitude of the sample is large.
adaptiveMagnitudeSamples :: (AbstractMagnitude a) => Integer -> SamplingAlgorithm a
adaptiveMagnitudeSamples n c = resampleLoop (\xs -> if genericLength xs > n then Nothing else Just $ newSamples xs) $ linearSamples (max 1 $ n `div` 10) c
    where newSamples xs = let a = abstractAverage $ map intervalMagnitude xs
                              in flip concatMap xs $ \x -> if intervalMagnitude x >= a then splitInterval x else [x]
          intervalMagnitude :: (AbstractMagnitude a) => IntervalSample a -> Double
          intervalMagnitude (IntervalSample _ l h a) = magnitude a * (abs $ h-l)

-- | Loop to keep generating samples until finished.
resampleLoop :: (b -> Maybe b) -> b -> b
resampleLoop nextPass initial_value = f $ initial_value
    where f x = maybe x f $ nextPass x
