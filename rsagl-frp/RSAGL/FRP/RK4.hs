module RSAGL.FRP.RK4
    (rk4,
     integrateRK4,
     rk4',
     integrateRK4')
    where

import RSAGL.Math.AbstractVector
import RSAGL.FRP.Time

-- | Generic implementation of one time-step of the RK4 algorithm.
genericRK4 :: (AbstractVector v) =>
              (Time -> p -> v -> p) ->
              -- ^ Addition function.  Adds a vector to a point using
              -- a time-diff.  The input vector (@v@) to this function
              -- is already scaled to represent the time interval,
              -- so the time-diff should be ignored unless this
              -- function is to have non-linear with respect to
              -- frame rate.
              (Time -> p -> Rate v) ->
              -- ^ The differential equation, representing velocity
              -- in terms of position at an absolute time.
              p ->
              -- ^ Initial value.
              Time ->
              -- ^ Starting time.
              Time ->
              -- ^ Ending time.
              p
genericRK4 addPV diffF p0 t0 t1 =
    addPV h p0 $ (scalarMultiply (recip 6)
                                 (abstractSum [k1,k2,k2,k3,k3,k4])
                     `over` h)
    where k1 = diffF t0 p0
          k2 = diffF tmid (addPV h2 p0 $ k1 `over` h2)
          k3 = diffF tmid (addPV h2 p0 $ k2 `over` h2)
          k4 = diffF t1 (addPV h p0 $ k3 `over` h)
          h = t1 `sub` t0
          h2 = scalarMultiply (recip 2) h
          tmid = t0 `add` h2

-- | Implementation of RK4 that time steps a system in which velocity is
-- a function of absolute time and position.
rk4 :: (AbstractVector v) =>
       (p -> v -> p) ->
       -- ^ Definition of vector addition.
       (Time -> p -> Rate v) ->
       -- ^ Differential equation, representing velocity in terms
       -- of position at an absolute time.
       p ->
       -- ^ Initial value.
       Time ->
       -- ^ Starting time.
       Time ->
       -- ^ Ending time.
       p
rk4 addPV = genericRK4 (const addPV)

-- | Implementation of RK4 that time steps a system in which acceleration
-- is a function of absolute time, position and velocity.
rk4' :: (AbstractVector v) =>
        (p -> v -> p) ->
        -- ^ Definition of vector addition.
        (Time -> p -> Rate v -> Acceleration v) ->
        -- ^ Differential equation, representing acceleration in
        -- terms of position and velocity at an absolute time.
        (p,Rate v) ->
        -- ^ Initial value.
        Time ->
        -- ^ Starting time.
        Time ->
        -- ^ Ending time.
        (p,Rate v)
rk4' addPV diffF = genericRK4
    (\t (p,old_v) delta_v -> let new_v = old_v `add` delta_v
                                 in (addPV p $ (scalarMultiply (recip 2) $
                                               old_v `add` new_v)
                                        `over` t,new_v))
    (\t (p,v) -> diffF t p v)

-- | Integrate a system of multiple time steps.
genericIntegrate :: (p -> Time -> Time -> p) ->
                    -- ^ Description of a single time step,
                    -- given position, initial time, and ending time.
                    p ->
                    -- ^ Initial value.
                    Time ->
                    -- ^ Starting time.
                    Time ->
                    -- ^ Ending time.
                    Integer ->
                    -- ^ Number of time steps.
                    p
genericIntegrate _ pn _ _ 0 = pn
genericIntegrate f p0 t0 tn n = genericIntegrate f p1 t1 tn (n-1)
    where t1 = t0 `add` (scalarMultiply (recip $ fromInteger n) $ tn `sub` t0)
          p1 = f p0 t0 t1

-- | Implementation of RK4 that repeatedly time steps a system in which velocity
-- is a function of absolute time and position.
integrateRK4 :: (AbstractVector v) =>
                (p -> v -> p) ->
                -- ^ Definition of vector addition.
                (Time -> p -> Rate v) ->
                -- ^ Differential equation, representing velocity in terms
                -- of position at an absolute time.
                p ->
                -- ^ Initial value.
                Time ->
                -- ^ Starting time.
                Time ->
                -- ^ Ending time.
                Integer ->
                -- ^ Number of time steps.
                p
integrateRK4 addPV diffF = genericIntegrate $ rk4 addPV diffF

-- | Implementation of RK4 that repeatedly time steps a system in which
-- acceleration is a function of absolute time, position and velocity.
integrateRK4' :: (AbstractVector v) =>
                 (p -> v -> p) ->
                 -- ^ Definition of vector addition.
                 (Time -> p -> Rate v -> Acceleration v) ->
                 -- ^ Differential equation, representing acceleration in
                 -- terms of position and velocity at an absolute time.
                 (p,Rate v) ->
                 -- ^ Initial value.
                 Time ->
                 -- ^ Starting time.
                 Time ->
                 -- ^ Ending time.
                 Integer ->
                 -- ^ Number of time steps.
                 (p,Rate v)
integrateRK4' addPV diffF = genericIntegrate $ rk4' addPV diffF

