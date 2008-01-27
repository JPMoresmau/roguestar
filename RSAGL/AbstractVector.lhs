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
    magnitude = realToFrac

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
    magnitude = id

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

\subsection{Operations on Abstract Vectors}

\begin{code}
abstractSum :: (AbstractAdd p v,AbstractZero p) => [v] -> p
abstractSum = foldr (flip add) zero

abstractAverage :: (AbstractAdd p v,AbstractSubtract p v,AbstractVector v) => [p] -> p
abstractAverage vs = add fixed_point $ scalarMultiply (recip $ fromInteger $ genericLength vs) $ abstractSum $ map (`sub` fixed_point) vs
    where fixed_point = head vs

abstractDistance :: (AbstractMagnitude v,AbstractSubtract p v) => p -> p -> Double
abstractDistance x y = magnitude $ x `sub` y
\end{code}