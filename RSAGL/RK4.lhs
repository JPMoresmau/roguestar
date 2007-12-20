\section{Runge-Kutta}

Haskell implementation of RK4.

\begin{code}
module RSAGL.RK4
    (rk4,
     integrateRK4)
    where

import RSAGL.AbstractVector
import RSAGL.Time
\end{code}

\begin{code}
rk4 :: (AbstractVector v) => (p -> v -> p) -> (Time -> p -> Rate v) -> p -> Time -> Time -> p
rk4 addPV diffF p0 t0 t1 = p0 `addPV` (scalarMultiply (recip 6) (abstractSum [k1,k2,k2,k3,k3,k4]) `over` h)
    where k1 = diffF t0 p0
          k2 = diffF tmid (p0 `addPV` (k1 `over` h2))
          k3 = diffF tmid (p0 `addPV` (k2 `over` h2))
          k4 = diffF t1 (p0 `addPV` (k3 `over` h))
          h = t1 `sub` t0
          h2 = scalarMultiply (recip 2) h
          tmid = t0 `add` h2

integrateRK4 :: (AbstractVector v) => (p -> v -> p) -> (Time -> p -> Rate v) -> p -> Time -> Time -> Integer -> p
integrateRK4 _ _ pn _ _ 0 = pn
integrateRK4 addPV diffF p0 t0 tn n = integrateRK4 addPV diffF p1 t1 tn (n-1)
    where t1 = t0 `add` (scalarMultiply (recip $ realToFrac n) $ tn `sub` t0)
          p1 = rk4 addPV diffF p0 t0 t1
\end{code}