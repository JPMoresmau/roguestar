\section{Abstract Vectors}

The \texttt{AbstractVector} typeclass provides some basic operations sufficient to implement various generic numerical algorithms, including numerical integration.

\begin{code}
module RSAGL.AbstractVector
    (AbstractVector(..),
     abstractSum)
    where

import Data.Fixed
\end{code}

The abstract vector laws:

\texttt{v `add` abstract_zero = v}

\texttt{magnitude v `scalarMultiply` abstractNormalize v = v}

\texttt{magnitude (normalize v) = 1.0}

\texttt{d = x `sub` y} is the shortest vector, \texttt{d}, such that \texttt{y `add` d = x}.

\begin{code}
class AbstractVector v where
    zero :: v
    add :: v -> v -> v
    sub :: v -> v -> v
    scalarMultiply :: Double -> v -> v
    magnitude :: v -> Double
    normalize :: v -> v

instance AbstractVector Double where
    zero = 0
    add = (+)
    sub = (-)
    scalarMultiply = (*)
    magnitude = abs
    normalize = signum

instance (HasResolution a) => AbstractVector (Fixed a) where
    zero = 0
    add = (+)
    sub = (-)
    scalarMultiply d = (realToFrac d *)
    magnitude = realToFrac
    normalize = signum
\end{code}

\subsection{Operations on Abstract Vectors}

\begin{code}
abstractSum :: (AbstractVector av) => [av] -> av
abstractSum = foldr add zero
\end{code}