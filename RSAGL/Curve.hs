{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RSAGL.Curve
    (Curve,
     zipCurve,
     iterateCurve,
     transposeCurve,
     curve,
     Surface,
     surface,
     wrapSurface,
     unwrapSurface,
     pretransformCurve,
     pretransformCurve2,
     transposeSurface,
     zipSurface,
     iterateSurface,
     halfIterateSurface,
     pretransformSurface,
     flipTransposeSurface,
     uv_identity,
     surfaceDerivative,
     curveDerivative,
     orientationLoops,
     newellCurve,
     surfaceNormals3D)
    where

import Control.Arrow hiding (pure)
import RSAGL.Vector
import RSAGL.Angle
import RSAGL.Auxiliary
import RSAGL.Affine
import Data.List
import Data.Maybe
import Control.Parallel.Strategies
import Control.Applicative
import RSAGL.AbstractVector
import Debug.Trace
import RSAGL.BoundingBox

-- | A parametric function that is aware of it's own sampling interval.  The first parameter is the curve parameter, while the second is the sampling interval.
type CurveF a = (Double,Double) -> a
-- | A surface is a curve of curves.
type SurfaceF a = CurveF (CurveF a)
-- | A 'Curve' is a parametric function from a one-dimensional space into a space of an arbitrary datatype.  The key feature of a 'Curve' is that it is aware of it's own
-- sampling interval.  Using this information and appropriate subtraction and scalar multiplication functions provided by RSAGL.AbstractVector, a 'Curve' can be differentiated.
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

instance NFData (Curve a) where
    rnf (Curve f) = seq f ()

-- | Sample a specific point on a curve given the point on the curve (first parameter) and the sampling interval (second parameter).
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

-- | Transform the input parameter of a 'Curve'.
pretransformCurve :: (Double -> Double) -> Curve a -> Curve a
pretransformCurve g = mapCurve (\f (h,u) -> f (h,g u))

-- | Transform the input parameters of both axes of a surface 'Curve'.
pretransformCurve2 :: (Double -> Double) -> (Double -> Double) -> Curve (Curve a) -> Curve (Curve a)
pretransformCurve2 fu fv = mapCurve2 $ (\f u v -> f (second fu u) (second fv v))

-- | Transpose the inner and outer components of a curve.
transposeCurve :: Curve (Curve a) -> Curve (Curve a)
transposeCurve = mapCurve2 flip

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

-- | Transform the input parameters to a surface.
pretransformSurface :: (Double -> Double) -> (Double -> Double) -> Surface a -> Surface a
pretransformSurface fu fv = Surface . pretransformCurve2 fv fu . unwrapSurface

-- | Transpose a surface while flipping the inner curve, so that that orientable surfaces retain their orientation.
flipTransposeSurface :: Surface a -> Surface a
flipTransposeSurface = pretransformSurface id (1-) . transposeSurface

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
-- the problematic parametric inputs using 'pretransformSurface'.
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
