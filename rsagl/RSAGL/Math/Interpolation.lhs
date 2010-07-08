\section{RSAGL.Interpolation}

\begin{code}
module RSAGL.Math.Interpolation
    (lerp,
     lerpClamped,
     lerpBetween,
     lerpBetweenMutated,
     lerpBetweenClamped,
     lerpBetweenClampedMutated,
     lerp_mutator_continuous_1st,
     lerpMap)
    where

import RSAGL.Math.AbstractVector
import Data.Map as Map
import Data.Maybe
import RSAGL.Math.Types
\end{code}

\subsection{The Lerpable typeclass}

Implements linear interpolation.

\begin{code}
{-# INLINE lerp #-}
lerp :: (AbstractScale v,AbstractSubtract p v,AbstractAdd p v,RealFloat r) => r -> (p,p) -> p
lerp u (a,b) = a `add` scalarMultiply (f2f u) (b `sub` a)
\end{code}

\subsection{Non-linear interpolations}

The mutated versions of the lerp functions takes an arbitrary ``mutator function'' to define non-linear interpolation curves.
The ``between'' versions of the lerp functions allow the u-value to lie between any two numbers, as opposed to between 0 and 1.
The ``clamped'' versions of the lerp functions clamp the u-value to lie between its boundaries.  Otherwise, with non-clamped
interpolations, the u-value may lie outside of its boundaries.

\begin{code}
{-# INLINE lerpClamped #-}

lerpClamped :: (AbstractScale v,AbstractSubtract p v,AbstractAdd p v,RealFloat r) => r -> (p,p) -> p
lerpClamped u = lerpBetweenClamped (0,u,1)

{-# INLINE lerpBetween #-}
lerpBetween :: (AbstractScale v,AbstractSubtract p v,AbstractAdd p v,RealFloat r) => (r,r,r) -> (p,p) -> p
lerpBetween = lerpBetweenMutated id

{-# INLINE lerpBetweenMutated #-}
lerpBetweenMutated :: (AbstractScale v,AbstractSubtract p v,AbstractAdd p v,RealFloat r) => (r -> r) -> (r,r,r) -> (p,p) -> p
lerpBetweenMutated _ (l,_,r) | l == r = lerp 0.5
lerpBetweenMutated mutator (l,u,r) = lerp $ mutator $ (u-l) / (r-l)

{-# INLINE lerpBetweenClamped #-}
lerpBetweenClamped :: (AbstractScale v,AbstractSubtract p v,AbstractAdd p v,RealFloat r,Ord r) => (r,r,r) -> (p,p) -> p
lerpBetweenClamped = lerpBetweenClampedMutated id

{-# INLINE lerpBetweenClampedMutated #-}
lerpBetweenClampedMutated :: (AbstractScale v,AbstractSubtract p v,AbstractAdd p v,RealFloat r,Ord r) => (r -> r) -> (r,r,r) -> (p,p) -> p
lerpBetweenClampedMutated mutator (l,u,r) = lerpBetweenMutated (lerp_mutator_clamp . mutator) (l,u,r)
\end{code}

\subsection{Lerp mutators}

\texttt{lerp\_mutator\_clamp} implements clamping between 0 and 1.

\begin{code}
{-# INLINE lerp_mutator_clamp #-}

lerp_mutator_clamp :: (RealFloat r) => r -> r
lerp_mutator_clamp = min 1 . max 0
\end{code}

\texttt{lerp\_mutator\_continuous\_1st} implements clamping between 0 and 1, but such that the 1st derivative of the result is continuous.

\begin{code}
{-# INLINE lerp_mutator_continuous_1st #-}

lerp_mutator_continuous_1st :: (RealFloat r) => r -> r
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
{-# INLINE lerpMap #-}

lerpMap :: (RealFloat r,AbstractScale v,AbstractSubtract p v,AbstractAdd p v) => [(r,p)] -> r -> p
lerpMap pts = lerpMapSorted $ Map.fromList pts

{-# INLINE lerpMapSorted #-}
lerpMapSorted :: (RealFloat r,AbstractScale v,AbstractSubtract p v,AbstractAdd p v) => Map r p -> r -> p
lerpMapSorted m _ | Map.null m = error "lerpMapSorted: empty map"
lerpMapSorted m u = case () of
        () | isJust exactly -> fromJust exactly
        () | Map.null lowers -> higher_a
        () | Map.null highers -> lower_a
        () -> lerpBetween (lower_r,u,higher_r) (lower_a,higher_a)
    where (lowers, exactly, highers) = splitLookup u m
          (lower_r,lower_a) = findMax lowers
          (higher_r,higher_a) = findMin highers
\end{code}
