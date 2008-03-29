\section{RSAGL.ThreadedArrow}

A ThreadedArrow is an extension of the SwitchedArrow.  In addition to switching, ThreadedArrow threads can spawn new threads and
terminate themselves.  All threads recieve the same input.

ThreadedArrow also supports a model of thread management that allows an arbitrary, external function to view all threads and cull unwanted threads
either before they begin or sometime after, and even control the order in which threads exectute.

While the ThreadedArrow has conceptual similarities to the threading systems provided by an operating system,
it provides neither parallelism nor concurency.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.ThreadedArrow
    (ThreadIdentity,
     nullaryThreadIdentity,
     maybeThreadIdentity,
     unionThreadIdentity,
     ThreadedFunction,
     ThreadedArrow,
     RSAGL.ThreadedArrow.switchContinue,
     RSAGL.ThreadedArrow.switchTerminate,
     spawnThreads,
     killThreadIf,
     threadIdentity,
     RSAGL.ThreadedArrow.statefulForm)
    where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.StatefulArrow as StatefulArrow
import Data.Maybe
import Data.List

type ThreadIdentity t = forall i x o. i -> [(t,x)] -> [(t,(o,x))] -> [x]

nullaryThreadIdentity :: ThreadIdentity ()
nullaryThreadIdentity _ news olds = map (snd . snd) olds ++ map snd news

maybeThreadIdentity :: ThreadIdentity t -> ThreadIdentity (Maybe t)
maybeThreadIdentity manageThreads i news olds = manageThreads i (justs news) (justs olds) ++ nothings
        where nothings = map snd (filter (isNothing . fst) news) ++ map (snd . snd) (filter (isNothing . fst) olds)
	      justs = map (first fromJust) . filter (isJust . fst)

unionThreadIdentity :: (t -> t -> Bool) -> ThreadIdentity t
unionThreadIdentity predicate _ news olds_ = map snd $ unionBy (\x y -> fst x `predicate` fst y) olds news
    where olds = map (second snd) olds_

newtype ThreadedArrow t i o a j p = ThreadedArrow (SwitchedArrow i (Maybe o) (StateArrow (t,[(t,ThreadedArrow t i o a i (Maybe o))]) a) j p)
type ThreadedFunction i o j p = ThreadedArrow () i o (->) j p

instance (ArrowChoice a) => Arrow (ThreadedArrow t i o a) where
    (>>>) (ThreadedArrow ta1) (ThreadedArrow ta2) = ThreadedArrow $ ta1 >>> ta2
    arr = ThreadedArrow . arr
    first (ThreadedArrow f) = ThreadedArrow $ first f

instance (Arrow a,ArrowChoice a) => ArrowTransformer (ThreadedArrow t i o) a where
    lift = ThreadedArrow . lift . lift

instance (ArrowChoice a) => ArrowChoice (ThreadedArrow t i o a) where
    left (ThreadedArrow a) = ThreadedArrow $ left a

instance (ArrowChoice a,ArrowApply a) => ArrowApply (ThreadedArrow t i o a) where
    app = ThreadedArrow $ proc (ThreadedArrow a,b) -> app -< (a,b)

statefulForm :: (ArrowChoice a,ArrowApply a) => (forall x. i -> [(t,x)] -> [(t,(o,x))] -> [x]) -> [(t,ThreadedArrow t i o a i o)] -> StatefulArrow a i [(t,o)]
statefulForm manageThreads initial_threads = flip stateContext (map (second (>>> arr Just)) initial_threads) $ proc i ->
    do threads <- fetch -< ()
       olds <- lift (runThreadsLoop manageThreads) -< (i,[],threads)
       store -< map (second snd) olds
       returnA -< map (second fst) olds

manageOldNewThreads :: (forall ex. i -> [(t,ex)] -> [(t,(o,ex))] -> [ex]) -> i -> [(t,x)] -> [(t,(o,x))] -> ([(t,x)],[(t,(o,x))])
manageOldNewThreads manageThreads i news_ olds_ = (concat *** concat) $ unzip $ map (either (\l -> ([l],[])) (\r -> ([],[r]))) result
    where news = map (\x -> second (const $ Left x) x) news_
          olds = map (\x -> second (second $ const $ Right x) x) olds_
	  result = manageThreads i news olds

runThreadsLoop :: (ArrowChoice a,ArrowApply a) => 
                      (forall x. i -> [(t,x)] -> [(t,(o,x))] -> [x]) ->
		      a (i,[(t,(o,ThreadedArrow t i o a i (Maybe o)))],[(t,ThreadedArrow t i o a i (Maybe o))])
                        [(t,(o,ThreadedArrow t i o a i (Maybe o)))]
runThreadsLoop manageThreads = proc (i,finished_threads,current_threads) ->
    do (freshly_finished_threads,(_,freshly_spawned_threads)) <- runState runThreads -< 
               ((i,current_threads),(error "runThreadsLoop: undefined thread identity",[]))
       let (managed_spawned_threads,managed_finished_threads) = manageOldNewThreads manageThreads i 
                                                                freshly_spawned_threads 
								(freshly_finished_threads ++ finished_threads)
       if null managed_spawned_threads
           then returnA -< managed_finished_threads
	   else runThreadsLoop manageThreads -< (i,managed_finished_threads,managed_spawned_threads)

runThreads :: (ArrowChoice a,ArrowApply a) => StateArrow (t,[(t,ThreadedArrow t i o a i (Maybe o))]) a 
                                                         (i,[(t,ThreadedArrow t i o a i (Maybe o))]) 
							 [(t,(o,ThreadedArrow t i o a i (Maybe o)))]
runThreads = proc (i,threads) ->
    do case threads of
           [] -> returnA -< []
	   ((ident,ThreadedArrow switchedA):rest_in) ->
	       do x <- fetch -< ()
	          store -< first (const ident) x
	          (m_o,newA) <- app -< (runStateful switchedA,i)
	          rest_out <- runThreads -< (i,rest_in)
                  returnA -< (maybe id (\o -> ((ident,(o,ThreadedArrow newA)) :)) m_o) rest_out

switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) => ThreadedArrow t i o a (Maybe (ThreadedArrow t i o a i o),i) i
switchContinue = (arr $ first $ fmap (\(ThreadedArrow thread) -> thread >>> arr Just)) >>> (ThreadedArrow SwitchedArrow.switchContinue)

switchTerminate :: (Arrow a,ArrowChoice a) => ThreadedArrow t i o a (Maybe (ThreadedArrow t i o a i o),o) o
switchTerminate = proc (m_thread,o) ->
    do ThreadedArrow $ SwitchedArrow.switchTerminate -< (fmap (\(ThreadedArrow thread) -> thread >>> arr Just) m_thread,Just o)
       returnA -< o

spawnThreads :: (Arrow a,ArrowChoice a) => ThreadedArrow t i o a [(t,ThreadedArrow t i o a i o)] ()
spawnThreads = ThreadedArrow $ lift $ proc new_spawned -> 
    do x <- fetch -< ()
       store -< second (map (second (>>> arr Just)) new_spawned ++) x
       returnA -< ()

killThreadIf :: (Arrow a,ArrowChoice a,ArrowApply a) => ThreadedArrow t i o a Bool ()
killThreadIf = proc b -> 
    do ThreadedArrow SwitchedArrow.switchContinue -< (if b then (Just $ arr (const Nothing)) else Nothing,error "ThreadedArrow.killThreadIf: this thread has been killed")
       returnA -< ()

threadIdentity :: (ArrowChoice a) => ThreadedArrow t i o a () t
threadIdentity = arr fst <<< ThreadedArrow (lift fetch)
\end{code}
