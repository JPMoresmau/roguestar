{-# OPTIONS_GHC -fglasgow-exts -farrows #-}

module RSAGL.FRP
    (FRP,
     statefulFRP)
    where

import Data.Monoid
import qualified Data.Set as Set
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.StatefulArrow
import RSAGL.SwitchedArrow
import RSAGL.ThreadedArrow
import RSAGL.Time

type FRPThread s a i o = FRP s i o a i o
newtype FRP s i o a j p = FRP (StatefulArrow (ThreadedArrow i o (StateArrow (FRPState s a i o) a)) j p)

data FRPState s a i o = FRPState { frpstate_absolute_time :: Time,
                                   frpstate_delta_time :: Time,
                                   frpstate_switch :: StatefulArrow (StateArrow (FRPState s a i o) a) i o,
                                   frpstate_user :: s }

-- |
-- Given a starting set of FRP threads, generate a StatefulArrow that runs those threads.
--
statefulFRP :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => [FRPThread s a i o] -> s -> StatefulArrow a (i,Time) o
statefulFRP threads state = 
    let initial_state = FRPState { frpstate_absolute_time = undefined,  -- the absolute time is only available as an arrow input
                                   frpstate_delta_time = 0,
                                   frpstate_switch = makeStateful threads,
                                   frpstate_user = state }
        firstpass cont = StatefulArrow $
                         proc (i,t) -> do app -< (runStatefulArrow $ cont (initial_state { frpstate_absolute_time = t }),(i,t)) -- intercept and set the absolute time on the first pass
        in firstpass $ RSAGL.StatefulArrow.stateContext $
               proc (i,t) -> do context <- fetch -< ()
                                store -< context { frpstate_absolute_time = t,
                                                   frpstate_delta_time = t - frpstate_absolute_time context }
                                (o,next) <- app -< (runStatefulArrow $ frpstate_switch context,i)
                                store -< context { frpstate_switch = next }
                                returnA -< o

-- |
-- Embed one FRP inside another.
--
frp :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => [FRPThread s' a j p] -> s' -> FRP s i o a j p
frp threads s = FRP $ frp_ $ statefulFRP threads s
                    where frp_ f = StatefulArrow $ lift $ 
                                       proc i -> do t <- arr frpstate_absolute_time <<< fetch -< ()
                                                    (o,next) <- lift $ runStatefulArrow f -< (i,t)
                                                    returnA -< (o,frp_ next)

makeStateful :: (ArrowChoice a,ArrowApply a,Monoid o) => [FRPThread s a i o] -> StatefulArrow (StateArrow (FRPState s a i o) a) i o
makeStateful = threadedContext . map (\(FRP thread) -> statefulThread thread)

statefulThread :: (Arrow a,ArrowChoice a) => StatefulArrow (ThreadedArrow i o a) i o -> ThreadedArrow i o a i o
statefulThread (StatefulArrow sf) = 
    proc b -> do (c,sf') <- sf -< b
                 switchTerminate -< (statefulThread sf',c)

getAbsoluteTime :: (Arrow a,ArrowChoice a) => FRP s i o a () Time
getAbsoluteTime = arr frpstate_absolute_time <<< (FRP $ lift $ lift $ fetch)

class ReturnableMonoid o where
    returnA1 :: (Arrow a,ArrowChoice a,Monoid (o t)) => FRP s i (o t) a t (o t)

instance ReturnableMonoid [] where
    returnA1 = arr (\x -> [x])

instance ReturnableMonoid Set.Set where
    returnA1 = arr (Set.singleton)

instance (Arrow a,ArrowChoice a) => (ArrowState s (FRP s i o a)) where
    fetch = (arr frpstate_user <<<) $ FRP $ lift $ lift $ fetch
    store = proc x -> do s <- FRP $ lift $ lift fetch -< ()
                         FRP $ lift $ lift $ store -< s { frpstate_user = x }

instance (ArrowChoice a) => Arrow (FRP s i o a) where
    (>>>) (FRP x) (FRP y) = FRP $ x >>> y
    arr = FRP . arr
    first (FRP f) = FRP $ first f

instance (ArrowChoice a) => ArrowTransformer (FRP s i o) a where
    lift = FRP . lift . lift . lift

instance ArrowSwitched (FRP s) where
    switchContinue = proc (FRP thread,i) -> do FRP $ lift $ switchContinue -< (statefulThread thread,i)
    switchTerminate = proc (FRP thread,o) -> do FRP $ lift $ switchTerminate -< (statefulThread thread,o)

instance ArrowThreaded (FRP s) where
    spawnThreads = (FRP $ lift $ spawnThreads) <<< arr (map (\(FRP thread) -> statefulThread thread))
    killThread = FRP $ lift killThread
