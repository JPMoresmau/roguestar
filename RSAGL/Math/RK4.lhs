\section{Runge-Kutta}

Haskell implementation of RK4.

\begin{code}
module RSAGL.Math.RK4
    (rk4,
     integrateRK4,
     rk4',
     integrateRK4')
    where

import RSAGL.Math.AbstractVector
import RSAGL.FRP.Time
\end{code}

\begin{code}
genericRK4 :: (AbstractVector v) => (Time -> p -> v -> p) -> (Time -> p -> Rate v) -> p -> Time -> Time -> p
genericRK4 addPV diffF p0 t0 t1 = addPV h p0 $ (scalarMultiply (recip 6) (abstractSum [k1,k2,k2,k3,k3,k4]) `over` h)
    where k1 = diffF t0 p0
          k2 = diffF tmid (addPV h2 p0 $ k1 `over` h2)
          k3 = diffF tmid (addPV h2 p0 $ k2 `over` h2)
          k4 = diffF t1 (addPV h p0 $ k3 `over` h)
          h = t1 `sub` t0
          h2 = scalarMultiply (recip 2) h
          tmid = t0 `add` h2

rk4 :: (AbstractVector v) => (p -> v -> p) -> (Time -> p -> Rate v) -> p -> Time -> Time -> p
rk4 addPV = genericRK4 (const addPV)

rk4' :: (AbstractVector v) => (p -> v -> p) -> (Time -> p -> Rate v -> Acceleration v) -> (p,Rate v) -> Time -> Time -> (p,Rate v)
rk4' addPV diffF = genericRK4
    (\t (p,old_v) delta_v -> let new_v = old_v `add` delta_v in (addPV p $ (scalarMultiply (recip 2) $ old_v `add` new_v) `over` t,new_v))
    (\t (p,v) -> diffF t p v)

genericIntegrate :: (p -> Time -> Time -> p) -> p -> Time -> Time -> Integer -> p
genericIntegrate _ pn _ _ 0 = pn
genericIntegrate f p0 t0 tn n = genericIntegrate f p1 t1 tn (n-1)
    where t1 = t0 `add` (scalarMultiply (recip $ fromInteger n) $ tn `sub` t0)
          p1 = f p0 t0 t1

integrateRK4 :: (AbstractVector v) => (p -> v -> p) -> (Time -> p -> Rate v) -> p -> Time -> Time -> Integer -> p
integrateRK4 addPV diffF = genericIntegrate $ rk4 addPV diffF

integrateRK4' :: (AbstractVector v) => (p -> v -> p) -> (Time -> p -> Rate v -> Acceleration v) -> (p,Rate v) -> Time -> Time -> Integer -> (p,Rate v)
integrateRK4' addPV diffF = genericIntegrate $ rk4' addPV diffF
\end{code}
