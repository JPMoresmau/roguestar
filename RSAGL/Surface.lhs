\section{Surfaces}

A surface is a two-dimensional figure in an arbitrary space.  The Surface typeclass allows an arbitrary surface to be sampled so that
it can be rendered iteratively in OpenGL.  The Orientable typeclass allows a surface to be transformed into a surface of its normal
vectors.

\begin{code}

{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Surface
   (Surface,
    surface,
    transposeSurface,
    iterateSurface,
    halfIterateSurface,
    zipSurface,
    surfaceDerivative,
    curvewiseDerivative,
    uv_identity)
   where

import RSAGL.Affine
import RSAGL.Curve
import Data.List
import Control.Applicative
\end{code}

\begin{code}

data Surface a = Surface (Curve (Curve a)) (Curve (Curve a))

surface :: (Double -> Double -> a) -> Surface a
surface f = Surface (curve (curve . flip f)) (curve $ (curve . f))

iterateSurface :: (Integer,Integer) -> Surface a -> [[a]]
iterateSurface (u,v) (Surface s _) = map (iterateCurve u) $ iterateCurve v s

halfIterateSurface :: Integer -> Surface a -> [Curve a]
halfIterateSurface n (Surface s _) = iterateCurve n s

transposeSurface :: Surface a -> Surface a
transposeSurface (Surface uv vu) = Surface vu uv

instance Functor Surface where
    fmap f (Surface uv vu) = Surface (fmap (fmap f) $ uv) (fmap (fmap f) $ vu)

instance Applicative Surface where
    pure a = surface $ const $ const a
    f <*> a = zipSurface ($) f a

instance (AffineTransformable a) => AffineTransformable (Surface a) where
    scale v = fmap (scale v)
    translate v = fmap (translate v)
    rotate vector angle = fmap (rotate vector angle)
    transform m = fmap (transform m)

zipSurface :: (x -> y -> z) -> Surface x -> Surface y -> Surface z
zipSurface f (Surface xuv xvu) (Surface yuv yvu) =
    Surface (zipCurve (zipCurve f) xuv yuv) (zipCurve (zipCurve f) xvu yvu)

surfaceDerivative :: (CurveDifferentiable p v) => Surface p -> Surface (v,v)
surfaceDerivative s = zipSurface (,) (curvewiseDerivative s) (transposeSurface $ curvewiseDerivative $ transposeSurface s)

curvewiseDerivative :: (CurveDifferentiable p v) => Surface p -> Surface v
curvewiseDerivative (Surface uv vu) = Surface (curveDerivative uv) (fmap curveDerivative vu)

uv_identity :: Surface (Double,Double)
uv_identity = surface (,)
\end{code}
