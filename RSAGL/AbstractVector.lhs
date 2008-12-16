\section{Abstract Vectors}

The \texttt{AbstractVector} typeclass provides some basic operations sufficient to implement various generic numerical algorithms, including numerical integration.

\begin{code}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module RSAGL.AbstractVector
    (AbstractVector,
     AbstractZero(..),
     AbstractAdd(..),
     AbstractSubtract(..),
     AbstractScale(..),
     AbstractMagnitude(..),
     abstractScaleTo,
     abstractSum,
     abstractAverage,
     abstractDistance)
    where

import Data.Fixed
import Data.List
import Control.Applicative
import RSAGL.ApplicativeWrapper
\end{code}

\subsection{Abstract Vectors and Differences}

\begin{code}
class AbstractZero a where
    zero :: a

class AbstractAdd p v | p -> v where
    add :: p -> v -> p

class AbstractSubtract p v | p -> v where
    sub :: p -> p -> v

class AbstractScale v where
    scalarMultiply :: Double -> v -> v

class AbstractMagnitude v where
    magnitude :: v -> Double

class (AbstractZero v,AbstractAdd v v,AbstractSubtract v v,AbstractScale v) => AbstractVector v where
\end{code}

\subsection{Instances for Float, Double, and Fixed}

\begin{code}
instance AbstractZero Float where
    zero = 0

instance AbstractAdd Float Float where
    add = (+)

instance AbstractSubtract Float Float where
    sub = (-)

instance AbstractScale Float where
    scalarMultiply d = (realToFrac d *)

instance AbstractMagnitude Float where
    magnitude = abs . realToFrac

instance AbstractVector Float

instance AbstractZero Double where
    zero = 0

instance AbstractAdd Double Double where
    add = (+)

instance AbstractSubtract Double Double where
    sub = (-)

instance AbstractScale Double where
    scalarMultiply d = (realToFrac d *)

instance AbstractMagnitude Double where
    magnitude = abs

instance AbstractVector Double

instance (HasResolution a) => AbstractZero (Fixed a) where
    zero = 0

instance (HasResolution a) => AbstractAdd (Fixed a) (Fixed a) where
    add = (+)

instance (HasResolution a) => AbstractSubtract (Fixed a) (Fixed a) where
    sub = (-)

instance (HasResolution a) => AbstractScale (Fixed a) where
    scalarMultiply d = (realToFrac d *)

instance (HasResolution a) => AbstractMagnitude (Fixed a) where
    magnitude = realToFrac

instance (HasResolution a) => AbstractVector (Fixed a)

instance (Applicative f,AbstractZero p) => AbstractZero (ApplicativeWrapper f p) where
    zero = pure zero

instance (Applicative f,AbstractAdd p v) => AbstractAdd (ApplicativeWrapper f p) (ApplicativeWrapper f v) where
    add p v = add <$> p <*> v

instance (Applicative f,AbstractSubtract p v) => AbstractSubtract (ApplicativeWrapper f p) (ApplicativeWrapper f v) where
    sub x y = sub <$> x <*> y

instance (Applicative f,AbstractScale v) => AbstractScale (ApplicativeWrapper f v) where
    scalarMultiply d v = scalarMultiply d <$> v

instance (Applicative f,AbstractVector v) => AbstractVector (ApplicativeWrapper f v)
\end{code}

\subsection{Instances for Tuples}

\begin{code}
instance (AbstractZero a,AbstractZero b) => AbstractZero (a,b) where
    zero = (zero,zero)

instance (AbstractAdd a a',AbstractAdd b b') => AbstractAdd (a,b) (a',b') where
    add (a,b) (c,d) = (add a c,add b d)

instance (AbstractSubtract a a',AbstractSubtract b b') => AbstractSubtract (a,b) (a',b') where
    sub (a,b) (c,d) = (sub a c,sub b d)

instance (AbstractScale a,AbstractScale b) => AbstractScale (a,b) where
    scalarMultiply d (a,b) = (scalarMultiply d a,scalarMultiply d b)

instance (AbstractMagnitude a,AbstractMagnitude b) => AbstractMagnitude (a,b) where
    magnitude (a,b) = sqrt $ magnitude a ^ 2 + magnitude b ^ 2

instance (AbstractVector a,AbstractVector b) => AbstractVector (a,b)
\end{code}

\subsection{Instances for Lists}

\begin{code}
instance (AbstractScale a) => AbstractScale [a] where
    scalarMultiply d = map (scalarMultiply d)
\end{code}

\subsection{Operations on Abstract Vectors}

\begin{code}
abstractScaleTo :: (AbstractScale v,AbstractMagnitude v) => Double -> v -> v
abstractScaleTo _ v | magnitude v == 0 = v
abstractScaleTo x v = scalarMultiply (x / magnitude v) v

abstractSum :: (AbstractAdd p v,AbstractZero p) => [v] -> p
abstractSum = foldr (flip add) zero

abstractAverage :: (AbstractAdd p v,AbstractSubtract p v,AbstractVector v,AbstractZero p) => [p] -> p
abstractAverage vs = zero `add` scalarMultiply (recip $ fromInteger total_count) total_sum
    where f y (i,x) = i `seq` x `seq` (i+1,y `add` x)
          (total_count,total_sum) = foldr f (0,zero) $ map (`sub` zero) vs

abstractDistance :: (AbstractMagnitude v,AbstractSubtract p v) => p -> p -> Double
abstractDistance x y = magnitude $ x `sub` y
\end{code}
