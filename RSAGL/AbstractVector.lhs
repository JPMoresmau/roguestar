\section{Abstract Vectors}

The \texttt{AbstractVector} typeclass provides some basic operations sufficient to implement various generic numerical algorithms, including numerical integration.

\begin{code}
module RSAGL.AbstractVector
    (AbstractVector(..),
     abstractSum)
    where

import Data.Fixed
\end{code}

\begin{code}
class AbstractVector v where
    zero :: v
    add :: v -> v -> v
    sub :: v -> v -> v
    scalarMultiply :: Double -> v -> v

instance AbstractVector Double where
    zero = 0
    add = (+)
    sub = (-)
    scalarMultiply = (*)

instance (HasResolution a) => AbstractVector (Fixed a) where
    zero = 0
    add = (+)
    sub = (-)
    scalarMultiply d = (realToFrac d *)
\end{code}

\subsection{Operations on Abstract Vectors}

\begin{code}
abstractSum :: (AbstractVector av) => [av] -> av
abstractSum = foldr add zero
\end{code}