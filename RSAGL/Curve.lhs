\section{Curves}

A curve is a one-dimensional figure in an arbitrary space.  The Curve typeclass allows an arbitrary curve to be sampled so that
it can be rendered iteratively in OpenGL.  The Differentiable typeclass allows a Curve to be transformed into its derivative.

\begin{code}
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
     surfaceNormals3D)
    where

import Control.Arrow hiding (pure)
import RSAGL.Vector
import RSAGL.Auxiliary
import RSAGL.Affine
import Data.List
import Control.Parallel.Strategies
import Control.Applicative
import RSAGL.AbstractVector
\end{code}

\subsection{The Curve}

A \texttt{Curve} is either a simple parametric curve, or a derivative of a parametric curve.  To approximate the derivative of a curve, we require a subtraction function that yields the derivative's type and a scalar multiplication function that operates on the base type.

We also allow two curves to be zipped together.

We can take the derivative of a curve an arbitrary number of times, but this will run up against the precision of the underlying data types, including Double.

\begin{code}
type CurveF a = (Double,Double) -> a
type SurfaceF a = CurveF (CurveF a)
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

sampleCurve :: Curve a -> Double -> Double -> a
sampleCurve (Curve f) = curry f

iterateCurve :: Integer -> Curve x -> [x]
iterateCurve n c = map f $ zeroToOne n
    where f = sampleCurve c (0.25/fromInteger n)

zipCurve :: (x -> y -> z) -> Curve x -> Curve y -> Curve z
zipCurve f (Curve x) (Curve y) = Curve $ \hu -> f (x hu) (y hu)

mapCurve :: (CurveF a -> CurveF a) -> Curve a -> Curve a
mapCurve f = Curve . f . fromCurve

mapCurve2 :: (SurfaceF a -> SurfaceF a) -> Curve (Curve a) -> Curve (Curve a)
mapCurve2 f = Curve . (Curve .) . f . (fromCurve .) . fromCurve

pretransformCurve :: (Double -> Double) -> Curve a -> Curve a
pretransformCurve g = mapCurve (\f (h,u) -> f (h,g u))

pretransformCurve2 :: (Double -> Double) -> (Double -> Double) -> Curve (Curve a) -> Curve (Curve a)
pretransformCurve2 fx fy = mapCurve2 $ (\f x y -> f (second fx x) (second fy y))

transposeCurve :: Curve (Curve a) -> Curve (Curve a)
transposeCurve = mapCurve2 flip

curve :: (Double -> a) -> Curve a
curve = Curve . uncurry . const
\end{code}

\subsection{Surfaces}

\begin{code}
newtype Surface a = Surface (Curve (Curve a)) deriving (NFData,AffineTransformable)

surface :: (Double -> Double -> a) -> Surface a
surface f = Surface $ curve (\x -> curve $ flip f x)

wrapSurface :: Curve (Curve a) -> Surface a
wrapSurface = Surface

unwrapSurface :: Surface a -> Curve (Curve a)
unwrapSurface (Surface s) = s

transposeSurface :: Surface a -> Surface a
transposeSurface (Surface s) = Surface $ transposeCurve s

iterateSurface :: (Integer,Integer) -> Surface a -> [[a]]
iterateSurface (u,v) (Surface s) = map (iterateCurve u) $ iterateCurve v s

halfIterateSurface :: Integer -> Surface a -> [Curve a]
halfIterateSurface u = iterateCurve u . unwrapSurface

instance Functor Surface where
    fmap f (Surface x) = Surface $ fmap (fmap f) x

instance Applicative Surface where
    pure a = surface (const $ const a)
    f <*> a = zipSurface ($) f a

zipSurface :: (x -> y -> z) -> Surface x -> Surface y -> Surface z
zipSurface f (Surface x) (Surface y) = Surface $ zipCurve (zipCurve f) x y

pretransformSurface :: (Double -> Double) -> (Double -> Double) -> Surface a -> Surface a
pretransformSurface fx fy = Surface . pretransformCurve2 fx fy . unwrapSurface

flipTransposeSurface :: Surface a -> Surface a
flipTransposeSurface = pretransformSurface id (1-) . transposeSurface

uv_identity :: Surface (Double,Double)
uv_identity = surface (curry id)
\end{code}

\subsection{Taking the Derivative of a Curve}

\begin{code}
curveDerivative :: (AbstractSubtract p v,AbstractScale v) => Curve p -> Curve v
curveDerivative (Curve f) = Curve $ \(h,u) -> scalarMultiply (recip $ 2 * h) $ f (h/2,u+h) `sub` f (h/2,u-h)

surfaceDerivative :: (AbstractSubtract p v,AbstractScale v) => Surface p -> Surface (v,v)
surfaceDerivative s = zipSurface (,) (curvewiseDerivative s) (transposeSurface $ curvewiseDerivative $ transposeSurface s)
    where curvewiseDerivative (Surface t) = Surface $ fmap curveDerivative t

surfaceNormals3D :: Surface Point3D -> Surface SurfaceVertex3D
surfaceNormals3D s = SurfaceVertex3D <$> s <*> fmap (vectorNormalize . uncurry crossProduct) (surfaceDerivative s)
\end{code}
