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

instance ArrowSwitched ThreadedArrow where
    switchContinue ta@(ThreadedArrow cont) = proc i ->
        do (ThreadedArrow $ lift $ substituteThreadPrim ta) -< ()
           (ThreadedArrow $ switchContinuePrim cont) -< i
    switchTerminate ta@(ThreadedArrow cont) = proc o ->
        do (ThreadedArrow $ lift $ substituteThreadPrim ta) -< ()
           (ThreadedArrow $ switchTerminate cont) -< o

-- |
-- An arrow that has a monoid result can be split into threads, the results of which are
-- combined into a single result using mplus.
--
-- spawnThreads spawns the specified threads, which will run immidiately after this thread ends.
-- killThread ends the current thread immediately, returning the specified result.
--
class ArrowThreaded a where
    spawnThreads :: (Arrow b,ArrowChoice b) => [a i o b i o] -> a i o b () ()

    killThread :: (Arrow b,ArrowChoice b,Monoid o) => a i o b o o

instance ArrowThreaded ThreadedArrow where
    spawnThreads = spawnThreadsPrim
    killThread = killThreadPrim

substituteThreadPrim :: (Arrow a) => ThreadedArrow i o a i o -> StateArrow (ThreadInfo a i o) a () ()
substituteThreadPrim thread = fetch >>> arr substituteThreadPrim_ >>> store
    where substituteThreadPrim_ thread_info = thread_info { ti_active_thread = Just thread }

spawnThreadsPrim :: (Arrow a,ArrowChoice a) => [ThreadedSwitch a i o] -> ThreadedArrow i o a () ()
spawnThreadsPrim threads = ThreadedArrow $ lift $ fetch >>> arr spawnThreadsPrim_ >>> store
    where spawnThreadsPrim_ thread_info = thread_info { ti_waiting_threads = threads ++ ti_waiting_threads thread_info }

-- |
-- Spawn the specified threads, but run them one step after this thread finishes.
-- This can prevent certain out-of-control loops when threads continuously spawn new threads.
--
spawnThreadsDelayed :: (Arrow (a i o b),ArrowThreaded a,ArrowSwitched a, Arrow b,ArrowChoice b,Monoid o) => [a i o b i o] -> a i o b () ()
spawnThreadsDelayed threads = spawnThreads $ map delayThread_ threads
    where delayThread_ thread = proc _ -> do switchTerminate thread -< mempty

killThreadPrim :: (Arrow a,ArrowChoice a,Monoid o) => ThreadedArrow i o a o o
killThreadPrim = proc o ->
    do switchTerminate (ThreadedArrow $ lift $ fetch >>> arr killThreadPrim_ >>> store >>> arr (const mempty)) -< o
           where killThreadPrim_ thread_info = thread_info { ti_active_thread = Nothing }

initialThreadedState :: [ThreadedSwitch a b c] -> ThreadInfo a b c
initialThreadedState threads = ThreadInfo { ti_active_thread = Nothing,
                                            ti_waiting_threads = threads,
                                            ti_completed_threads = [] }

threadedContext :: (Monoid c,ArrowApply a,ArrowChoice a) => [ThreadedSwitch a b c] -> StatefulArrow a b c
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