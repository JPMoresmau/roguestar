\section{Curves}

A curve is a one-dimensional figure in an arbitrary space.  The Curve typeclass allows an arbitrary curve to be sampled so that
it can be rendered iteratively in OpenGL.  The Differentiable typeclass allows a Curve to be transformed into its derivative.

\begin{code}

{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module RSAGL.Curve
    (Curve,
     zipCurve,
     iterateCurve,
     curve,
     CurveDifferentiable,
     curveDerivative)
    where

import RSAGL.Vector
import RSAGL.Auxiliary
import RSAGL.Affine
import Data.List
import Control.Parallel.Strategies
import Control.Applicative
\end{code}

\subsection{The Curve}

A \texttt{Curve} is either a simple parametric curve, or a derivative of a parametric curve.  To approximate the derivative of a curve, we require a subtraction function that yields the derivative's type and a scalar multiplication function that operates on the base type.

We also allow two curves to be zipped together.

We can take the derivative of a curve an arbitrary number of times, but this will run up against the precision of the underlying data types, including Double.

\begin{code}
data Curve a =
    Curve (Double -> a)
  | forall p. Derivative (p -> p -> a) (Double -> p -> p) (Curve p)
  | forall x y. Zip (x -> y -> a) (Curve x) (Curve y)

instance Functor Curve where
    fmap f (Curve c) = Curve $ f . c
    fmap f (Derivative sub sca c) = Derivative (\x y -> f $ sub x y) sca c
    fmap f (Zip g c1 c2) = Zip (\x y -> f $ g x y) c1 c2

instance Applicative Curve where
    pure a = curve $ const a
    f <*> a = zipCurve ($) f a

instance (AffineTransformable a) => AffineTransformable (Curve a) where
    scale v = fmap (scale v)
    translate v = fmap (translate v)
    rotate vector angle = fmap (rotate vector angle)
    transform m = fmap (transform m)

instance NFData (Curve a) where
    rnf (Curve f) = seq f ()
    rnf (Derivative sub sca src) = sub `seq` sca `seq` rnf src
    rnf (Zip f x y) = f `seq` rnf (x,y)

zipCurve :: (x -> y -> z) -> Curve x -> Curve y -> Curve z
zipCurve f (Curve x) (Curve y) = Curve $ (\n -> f (x n) (y n))
zipCurve f x y = Zip f x y

iterateCurve :: Integer -> Curve x -> [x]
iterateCurve n (Curve f) = map f $ zeroToOne n
iterateCurve n d@(Derivative {}) = map (\u -> sampleCurve u (0.25/fromInteger n) d) $ zeroToOne n
iterateCurve n (Zip f x y) = zipWith f (iterateCurve n x) (iterateCurve n y)

sampleCurve :: Double -> Double -> Curve x -> x
sampleCurve at _ (Curve f) = f at
sampleCurve at precision (Derivative sub sca src) =
    (sca (0.5/precision) $ sampleCurve (at+precision) (precision/2) src) `sub`
    (sca (0.5/precision) $ sampleCurve (at-precision) (precision/2) src)
sampleCurve at precision (Zip f x y) = f (sampleCurve at precision x) (sampleCurve at precision y)

curve :: (Double -> a) -> Curve a
curve = Curve
\end{code}

\subsection{Taking the Derivative of a Curve}

If a type implements \texttt{CurveDifferentiable}, then we can use the \texttt{curveDerivative} function to take the derivative of a curve of that type.

\begin{code}
class CurveDifferentiable p v | p -> v where
    differentiableSubtract :: p -> p -> v
    differentiableScale :: Double -> p -> p

instance CurveDifferentiable Double Double where
    differentiableSubtract = (-)
    differentiableScale = (*)

instance CurveDifferentiable Point3D Vector3D where
    differentiableSubtract = vectorToFrom
    differentiableScale = scale'

instance (CurveDifferentiable p v) => CurveDifferentiable (Curve p) (Curve v) where
    differentiableSubtract x y = zipCurve (differentiableSubtract) x y
    differentiableScale x c = fmap (differentiableScale x) c

curveDerivative :: (CurveDifferentiable p v) => Curve p -> Curve v
curveDerivative c = Derivative differentiableSubtract differentiableScale c
\end{code}
