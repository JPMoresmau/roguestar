{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.ThreadedArrow
    (ThreadedFunction,
     ThreadedSwitch,
     ThreadedArrow,
     ArrowThreaded(..),
     threadedContext,
     spawnThreadsDelayed)
    where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.SwitchedArrow
import RSAGL.StatefulArrow
import Data.Monoid
import Data.Maybe

type ThreadedSwitch a i o = ThreadedArrow i o a i o

type ThreadedFunction i o = ThreadedArrow i o (->)

data ThreadInfo a i o = ThreadInfo { ti_active_thread :: Maybe (ThreadedSwitch a i o),
                                     ti_waiting_threads :: [ThreadedSwitch a i o],
                                     ti_completed_threads :: [ThreadedSwitch a i o] }

newtype ThreadedArrow i o a j p = ThreadedArrow (SwitchedArrow i o (StateArrow (ThreadInfo a i o) a) j p)

instance (ArrowChoice a) => Arrow (ThreadedArrow i o a) where
    (>>>) (ThreadedArrow ta1) (ThreadedArrow ta2) = ThreadedArrow $ ta1 >>> ta2
    arr = ThreadedArrow . arr
    first (ThreadedArrow f) = ThreadedArrow $ first f

instance (Arrow a,ArrowChoice a) => ArrowTransformer (ThreadedArrow i o) a where
    lift = ThreadedArrow . lift . lift

instance (ArrowChoice a,ArrowApply a) => ArrowApply (ThreadedArrow i o a) where
    app = ThreadedArrow $ proc (ThreadedArrow a,b) -> app -< (a,b)

instance ArrowSwitched ThreadedArrow where
    switchContinue = proc (ta@(ThreadedArrow cont),i) ->
        do ThreadedArrow $ lift $ substituteThreadPrim -< ta
           (ThreadedArrow $ switchContinue) -< (cont,i)
    switchTerminate = proc (ta@(ThreadedArrow cont),o) ->
        do (ThreadedArrow $ lift $ substituteThreadPrim) -< ta
           (ThreadedArrow $ switchTerminate) -< (cont,o)

-- |
-- An arrow that has a monoid result can be split into threads, the results of which are
-- combined into a single result using mplus.
--
-- spawnThreads spawns the specified threads, which will run immidiately after this thread ends.
-- killThread ends the current thread immediately, returning the specified result.
--
class ArrowThreaded a where
    spawnThreads :: (Arrow b,ArrowChoice b) => a i o b [a i o b i o] ()
    killThread :: (Arrow b,ArrowChoice b,Monoid o) => a i o b o o

instance ArrowThreaded ThreadedArrow where
    spawnThreads = spawnThreadsPrim
    killThread = killThreadPrim

substituteThreadPrim :: (Arrow a) => StateArrow (ThreadInfo a i o) a (ThreadedSwitch a i o) ()
substituteThreadPrim = 
    proc thread -> do thread_info <- fetch -< ()
                      store -< thread_info { ti_active_thread = Just thread }

spawnThreadsPrim :: (Arrow a,ArrowChoice a) => ThreadedArrow i o a [ThreadedSwitch a i o] ()
spawnThreadsPrim = ThreadedArrow $ lift $ 
        proc threads -> do thread_info <- fetch -< ()
                           store -< thread_info { ti_waiting_threads = threads ++ ti_waiting_threads thread_info }

-- |
-- Spawn the specified threads, but run them one step after this thread finishes.
-- This can mitigate certain out-of-control loops when threads continuously spawn new threads.
--
spawnThreadsDelayed :: (Arrow (at i o a),ArrowThreaded at,ArrowSwitched at, Arrow a,ArrowChoice a,Monoid o) => at i o a [at i o a i o] ()
spawnThreadsDelayed = spawnThreads <<< (arr $ map delayThread_)
    where delayThread_ thread = proc _ -> do switchTerminate -< (thread,mempty)

killThreadPrim :: (Arrow a,ArrowChoice a,Monoid o) => ThreadedArrow i o a o o
killThreadPrim = proc o ->
    do switchTerminate -< (ThreadedArrow $ lift $ fetch >>> arr killThreadPrim_ >>> store >>> arr (const mempty), o)
           where killThreadPrim_ thread_info = thread_info { ti_active_thread = Nothing }

initialThreadedState :: [ThreadedSwitch a b c] -> ThreadInfo a b c
initialThreadedState threads = ThreadInfo { ti_active_thread = Nothing,
                                            ti_waiting_threads = threads,
                                            ti_completed_threads = [] }

threadedContext :: (Monoid c,ArrowChoice a,ArrowApply a) => [ThreadedSwitch a b c] -> StatefulArrow a b c
threadedContext = stateContext $
    proc i -> do threads <- fetch -< ()
                 (o,thread_result) <- lift (runState runThreads) -< (i,initialThreadedState threads)
                 store -< ti_completed_threads thread_result
		 returnA -< o

loadNextThread :: (Arrow a) => StateArrow (ThreadInfo a b c) a () (ThreadInfo a b c)
loadNextThread = fetch >>> arr loadNextThread_ >>> store >>> fetch
    where loadNextThread_ thread_info =
              case thread_info of
                  t@(ThreadInfo { ti_active_thread = Nothing, ti_waiting_threads = (thread:threads) }) -> 
                           (t { ti_active_thread = Just thread,
                                ti_waiting_threads = threads })
                  t -> t

storeCurrentThread :: (Arrow a) => StateArrow (ThreadInfo a b c) a () ()
storeCurrentThread = fetch >>> arr storeCurrentThread_ >>> store
    where storeCurrentThread_ thread_info =
              thread_info { ti_completed_threads = maybeToList (ti_active_thread thread_info) ++ ti_completed_threads thread_info,
                            ti_active_thread = Nothing }

runThread :: (Arrow a,ArrowChoice a) => ThreadedSwitch a i o -> StateArrow (ThreadInfo a i o) a i o
runThread (ThreadedArrow thread) = 
    let (StatefulArrow switched_form) = switchedContext thread
        in proc i -> do (o,_) <- switched_form -< i
                        storeCurrentThread -< ()
                        returnA -< o

runThreads :: (Monoid o,Arrow a,ArrowApply a,ArrowChoice a) => StateArrow (ThreadInfo a i o) a i o
runThreads = proc i ->
    do thread_info <- loadNextThread -< ()
       case ti_active_thread thread_info of
           Just thread -> do o <- app -< (runThread thread,i)
                             os <- runThreads -< i
                             returnA -< o `mappend` os
           Nothing -> returnA -< mempty