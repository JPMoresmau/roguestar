{-# LANGUAGE Arrows #-}

module RSAGL.FRP.Accumulation
    (delay,
     integral,
     derivative,
     accumulateNumerical,
     integralRK4,
     integralRK4',
     summation,
     threadTime,
     sticky,
     initial,
     EdgeDetectionMode(..),
     edge,
     changed,
     clingy)
    where

import RSAGL.FRP.FRP
import RSAGL.FRP.Time
import RSAGL.FRP.RK4
import System.Mem.StableName
import Control.Arrow
import RSAGL.Math.AbstractVector
import Data.Maybe

-- | Delay a piece of data for one frame.
delay :: x -> FRP e m x x
delay initial_value = accumulate (initial_value,error "delay: impossible") (\new_value (old_value,_) -> (new_value,old_value)) >>> arr snd

-- | Take the integral of a rate over time, using the trapezoidal rule.
integral :: (AbstractVector v,AbstractAdd p v) => p -> FRP e m (Rate v) p
integral initial_value = proc v ->
    do delta_t <- deltaTime -< ()
       (new_accum,_) <- accumulate (zero,perSecond zero) (\(delta_t,new_rate) (old_accum,old_rate) ->
           (old_accum `add` ((scalarMultiply (recip 2) $ new_rate `add` old_rate) `over` delta_t),new_rate)) -< (delta_t,v)
       returnA -< initial_value `add` new_accum

-- | Take the derivative of a value over time, by simple subtraction between frames.
derivative :: (AbstractVector v,AbstractSubtract p v) => FRP e m p (Rate v)
derivative = proc new_value ->
    do delta_t <- deltaTime -< ()
       m_old_value <- delay Nothing -< Just new_value
       let z = perSecond zero
       returnA -< maybe z (\old_value -> if delta_t == zero then z else (new_value `sub` old_value) `per` delta_t) m_old_value

-- | 'accumulate' harness for some numerical methods.
-- Parameters are: current input, previous output, delta time, absolute time, and number of frames at the specified frequency.
accumulateNumerical :: Frequency -> (i -> o -> Time -> Time -> Integer -> o) -> o -> FRP e m i o
accumulateNumerical frequency accumF initial_value = proc i ->
    do absolute_time <- absoluteTime -< ()
       delta_t <- deltaTime -< ()
       accumulate initial_value (\(i,absolute_time',delta_t',frames) o -> accumF i o absolute_time' delta_t' frames) -< 
           (i,absolute_time,delta_t,ceiling $ toSeconds delta_t / toSeconds (interval frequency))

integralRK4 :: (AbstractVector v) => Frequency -> (p -> v -> p) -> p -> FRP e m (Time -> p -> Rate v) p
integralRK4 f addPV = accumulateNumerical f (\diffF p abs_t delta_t -> integrateRK4 addPV diffF p (abs_t `sub` delta_t) abs_t)

integralRK4' :: (AbstractVector v) => Frequency -> (p -> v -> p) -> (p,Rate v) -> 
                FRP e m (Time -> p -> Rate v -> Acceleration v) (p,Rate v)
integralRK4' f addPV = accumulateNumerical f (\diffF p abs_t delta_t -> integrateRK4' addPV diffF p (abs_t `sub` delta_t) abs_t)

-- | Sum some data frame-by-frame.
summation :: (AbstractAdd p v) => p -> FRP e m v p
summation initial_value = accumulate initial_value (\v p -> p `add` v)

-- | Elapsed time since the instantiation of this switch or thread.  Reset when a thread switches.
threadTime :: FRP e m () Time
threadTime = summation zero <<< deltaTime

-- | The edge detection mode.  If 'Discrete', detect edge between subsequent frames only.
-- If 'Fuzzy' detect edge since the most recent previous detected edge.
-- If 'HashedDiscrete', the comparison function is itself expensive, and the FRP runtime will compare by 'StableName's as a short-circuit optimization.
data EdgeDetectionMode = Fuzzy | Discrete | HashedDiscrete

-- | Answer the most recent input that satisfies the predicate.
-- Accepts an initial value, which need not itself satisfy the predicate.
--
-- This can be a performance optimization, if it prevents unecessary evaluation of an input.
sticky :: (x -> Bool) -> x -> FRP e m x x
sticky f x = accumulate x (\new_x old_x -> if f new_x then new_x else old_x)

-- | Answer the first input that ever passes through a function.
initial :: FRP e m x x
initial = accumulate Nothing (\new_x m_old_x -> Just $ fromMaybe new_x m_old_x) >>> arr (fromMaybe $ error "initial: impossible happened")

-- | Returns 'True' only during frames on which the input has changed, based on a user-specified equality predicate.
-- The predicate function takes the most recent input as its first parameter.
edge :: EdgeDetectionMode -> (x -> x -> Bool) -> FRP e m x Bool
edge Discrete predicateF = proc x ->
    do d_x <- delay Nothing -< Just x
       returnA -< maybe True (not . predicateF x) d_x
edge HashedDiscrete predicateF = proc x ->
    do x_stable <- ioAction makeStableName -< x
       stable_edge <- edge Discrete (==) -< x_stable
       edge Discrete (\(a_stable,a) (b_stable,b) -> if a_stable == b_stable then True else predicateF a b) -< (stable_edge,x)
edge Fuzzy predicateF = arr snd <<< accumulate (Nothing,error "changed: impossible")
                                    (\x_now (x_old,_) -> if maybe True (predicateF x_now) x_old
                                                         then (x_old,False)
                                                         else (Just x_now,True))

-- | Same as 'edge Discrete'.
changed :: (x -> x -> Bool) -> FRP e m x Bool
changed = edge Discrete

-- | Recalculate a function only at the edges of it's input.
clingy :: EdgeDetectionMode -> (j -> j -> Bool) -> (j -> p) -> FRP e m j p
clingy edm predicateF f = proc j ->
    do e <- edge edm predicateF -< j
       arr snd <<< sticky fst (error "clingy: impossible") -< (e,f j)

