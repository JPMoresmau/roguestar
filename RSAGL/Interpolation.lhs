\section{RSAGL.Interpolation}

\begin{code}
module RSAGL.Interpolation
    (Lerpable(..),
     lerpBetween,
     lerpBetweenMutated,
     lerpBetweenClamped,
     lerpBetweenClampedMutated,
     lerp_mutator_continuous_1st,
     lerpMap)
    where

import Control.Applicative
import RSAGL.ApplicativeWrapper
import RSAGL.Angle
import RSAGL.Vector
import RSAGL.AbstractVector
import Data.Ord
import Data.Map as Map
import Data.Maybe
\end{code}

\subsection{The Lerpable typeclass}

Lerpable data types are subject to linear interpolation.

The lerp function takes a $u$-value and two endpoints, $a$ and $b$.  If $u$ lies between 0 and 1, then the result lies between $a$ and $b$.
If $u = 0$ or $u = 1$, then the result is $a$ or $b$, respectively.  If $u < 0$, then the result is on the opposite side of $a$ from $b$.  If $u > 1$, then the result is on the opposite side of $b$ from $a$.

\begin{code}
class Lerpable a where
    lerp :: (Real r) => r -> (a,a) -> a

instance (Lerpable a,Lerpable b) => Lerpable (a,b) where
    lerp u ((a1,a2),(b1,b2)) = (lerp u (a1,b1),lerp u (a2,b2))

instance (Lerpable a,Lerpable b,Lerpable c) => Lerpable (a,b,c) where
    lerp u ((a1,a2,a3),(b1,b2,b3)) = (lerp u (a1,b1),lerp u (a2,b2),lerp u (a3,b3))
\end{code}

\subsection{Non-linear interpolations}

The mutated versions of the lerp functions takes an arbitrary ``mutator function'' to define non-linear interpolation curves.
The ``between'' versions of the lerp functions allow the u-value to lie between any two numbers, as opposed to between 0 and 1.
The ``clamped'' versions of the lerp functions clamp the u-value to lie between its boundaries.  Otherwise, with non-clamped
interpolations, the u-value may lie outside of its boundaries.

\begin{code}
lerpBetween :: (Lerpable a,Real r,Fractional r) => (r,r,r) -> (a,a) -> a
lerpBetween = lerpBetweenMutated id

lerpBetweenMutated :: (Lerpable a,Real r,Fractional r) => (r -> r) -> (r,r,r) -> (a,a) -> a
lerpBetweenMutated _ (l,_,r) | l == r = lerp 0.5
lerpBetweenMutated mutator (l,u,r) = lerp $ mutator $ (u-l) / (r-l)

lerpBetweenClamped :: (Lerpable a,Real r,Fractional r,Ord r) => (r,r,r) -> (a,a) -> a
lerpBetweenClamped = lerpBetweenClampedMutated id

lerpBetweenClampedMutated :: (Lerpable a,Real r,Fractional r,Ord r) => (r -> r) -> (r,r,r) -> (a,a) -> a
lerpBetweenClampedMutated mutator (l,u,r) = lerpBetweenMutated (lerp_mutator_clamp . mutator) (l,u,r)
\end{code}

\subsection{Lerp mutators}

\texttt{lerp_mutator_clamp} implements clamping between 0 and 1.

\begin{code}
lerp_mutator_clamp :: (Real r) => r -> r
lerp_mutator_clamp = min 1 . max 0
\end{code}

\texttt{lerp_mutator_continuous_1st} implements clamping between 0 and 1, but such that the 1st derivative of the result is continuous.

\begin{code}
lerp_mutator_continuous_1st :: (Real r,Fractional r) => r -> r
lerp_mutator_continuous_1st x | x < 0 = 0
lerp_mutator_continuous_1st x | x > 1 = 1
lerp_mutator_continuous_1st x | x <= 0.5 = 2*x^2
lerp_mutator_continuous_1st x = 4*x - 2*x^2 - 1
\end{code}

\subsection{lerpMap}

Given many entities, lerp between the two entities closest to the given point
on either side.  For example, if we wanted to lerp between colors on a rainbow,
we might use the map [(0,red),(1,orange),(2,yellow),(3,green),(4,blue),(5,indigo),(6,violet)].
lerpMap 3.5 would result in a blue-green color.

\begin{code}
lerpMap :: (Real r,Fractional r,Lerpable a) => [(r,a)] -> r -> a
lerpMap pts = lerpMapSorted $ Map.fromList pts

lerpMapSorted :: (Real r,Fractional r,Lerpable a) => Map r a -> r -> a
lerpMapSorted m _ | Map.null m = error "lerpMapSorted: empty map"
lerpMapSorted m u = case () of
        () | isJust exactly -> fromJust exactly
        () | Map.null lowers -> higher_a
        () | Map.null highers -> lower_a
        () -> lerpBetween (lower_r,u,higher_r) (lower_a,higher_a)
    where (lowers, exactly, highers) = splitLookup u m
          (lower_r,lower_a) = findMax lowers
          (higher_r,higher_a) = findMin highers

lerpA :: (Applicative f,Lerpable a,Real r) => f r -> (f a,f a) -> f a
lerpA u (a,b) = (\u' -> curry (lerp u')) <$> u <*> a <*> b

lerpAPure :: (Applicative f,Lerpable a,Real r) => r -> (f a,f a) -> f a
lerpAPure u = lerpA (pure u)

lerpAbstract :: (AbstractVector v,Real r) => r -> (v,v) -> v
lerpAbstract u (a,b) = a `add` scalarMultiply (realToFrac u) (b `sub` a)

lerpXyz :: (Xyz xyz,Real r) => r -> (xyz,xyz) -> xyz
lerpXyz u (a,b) = fromXYZ $ lerp u (toXYZ a,toXYZ b)
\end{code}

\subsection{Lerpable data structures}

\begin{code}
instance Lerpable Double where
   {-# SPECIALIZE lerp :: Double -> (Double,Double) -> Double #-}
   lerp = lerpAbstract

instance Lerpable Float where
   {-# SPECIALIZE lerp :: Float -> (Float,Float) -> Float #-}
   lerp = lerpAbstract

instance Lerpable Vector3D where
   {-# SPECIALIZE lerp :: Double -> (Vector3D,Vector3D) -> Vector3D #-}
   lerp = lerpAbstract

instance Lerpable Point3D where
   {-# SPECIALIZE lerp :: Double -> (Point3D,Point3D) -> Point3D #-}
   lerp = lerpXyz

instance Lerpable Angle where
   {-# SPECIALIZE lerp :: Double -> (Angle,Angle) -> Angle #-}
   lerp = lerpAbstract

instance (Lerpable a) => Lerpable (Maybe a) where
   lerp = lerpAPure

instance (Lerpable a) => Lerpable ((->) r a) where
    lerp = lerpAPure

instance (Lerpable a,Applicative f) => Lerpable (ApplicativeWrapper f a) where
    lerp = lerpAPure
\end{code}
