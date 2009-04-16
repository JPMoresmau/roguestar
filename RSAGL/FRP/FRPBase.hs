{-# LANGUAGE Arrows, MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances, Rank2Types #-}

-- |  The 'FRPBase' arrow is a composite of the 'StatefulArrow' and 'ThreadedArrow' intended to support functional reactive programming.
module RSAGL.FRP.FRPBase
    (FRPBase,
     -- * Switching Operators
     RSAGL.FRP.FRPBase.switchContinue,
     RSAGL.FRP.FRPBase.switchTerminate,
     -- * Threading Operators
     RSAGL.FRP.FRPBase.spawnThreads,
     RSAGL.FRP.FRPBase.killThreadIf,
     RSAGL.FRP.FRPBase.threadIdentity,
     -- * Embedding One FRPBase Instance in Another
     RSAGL.FRP.FRPBase.frpBaseContext,     
     -- * Embedding Explicit Underlying State
     RSAGL.FRP.FRPBase.withState,
     RSAGL.FRP.FRPBase.withExposedState,
     -- * Embedding a StatefulArrow in an FRPBase Arrow
     RSAGL.FRP.FRPBase.statefulContext,
     -- * The StatefulArrow form of an FRPBase Arrow
     RSAGL.FRP.FRPBase.statefulForm)
    where

import Prelude hiding ((.),id)
import Control.Category
import Data.Monoid
import Control.Arrow
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.FRP.StatefulArrow as StatefulArrow
import RSAGL.FRP.ThreadedArrow as ThreadedArrow
import RSAGL.FRP.ArrowTransformerOptimizer

-- | FRPBase is a composite arrow in which the StatefulArrow is layered on top of the ThreadedArrow.
newtype FRPBase t i o a j p = FRPBase (
    ArrowTransformerOptimizer StatefulArrow (ThreadedArrow t i o a) j p)

fromFRPBase :: FRPBase t i o a j p -> (StatefulArrow (ThreadedArrow t i o a) j p)
fromFRPBase (FRPBase a) = collapseArrowTransformer a

instance (Category a,ArrowChoice a) => Category (FRPBase t i o a) where
    (.) (FRPBase lhs) (FRPBase rhs) = FRPBase $ lhs . rhs
    id = lift id

instance (ArrowChoice a) => Arrow (FRPBase t i o a) where
    arr = lift . arr
    first (FRPBase f) = FRPBase $ first f

instance (ArrowChoice a) => ArrowTransformer (FRPBase t i o) a where
    lift = FRPBase . lift . lift

switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPBase t i o a (Maybe (FRPBase t i o a i o),i) i
switchContinue = proc (thread,i) -> 
    do FRPBase $ lift $ ThreadedArrow.switchContinue -< (fmap (statefulThread . fromFRPBase) thread,i)

switchTerminate :: (Arrow a,ArrowChoice a) => FRPBase t i o a (Maybe (FRPBase t i o a i o),o) o
switchTerminate = proc (thread,o) -> 
    do FRPBase $ lift $ ThreadedArrow.switchTerminate -< (fmap (statefulThread . fromFRPBase) thread,o)

spawnThreads :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPBase t i o a [(t,FRPBase t i o a i o)] ()
spawnThreads = (FRPBase $ lift $ ThreadedArrow.spawnThreads) <<< 
               arr (map $ second $ statefulThread . fromFRPBase)

killThreadIf :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPBase t i o a Bool ()
killThreadIf = FRPBase $ lift ThreadedArrow.killThreadIf

threadIdentity :: (ArrowChoice a) => FRPBase t i o a () t
threadIdentity = FRPBase $ lift ThreadedArrow.threadIdentity


-- | The 'frpBaseContext' combinator allows a differently-typed 'FRPBase' thread group to be embedded in another.  
-- Using 'frpBaseContext', a thread can instantiate a group of threads that die whenever the calling thread 
-- dies or switches.  That is, the thread group is part of the state of the calling thread.
frpBaseContext :: (Arrow a,ArrowChoice a,ArrowApply a) => 
    (forall x. j -> [(t,x)] -> [(t,(p,x))] -> [x]) ->
    [(t,FRPBase t j p a j p)] -> FRPBase u i o a j [(t,p)]
frpBaseContext manageThreads threads = FRPBase $ raw $ statefulTransform lift (RSAGL.FRP.FRPBase.statefulForm manageThreads threads)

-- | See 'RSAGL.StatefulArrow.withState'.
withState :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => 
    (forall x. j -> [(t,x)] -> [(t,(p,x))] -> [x]) -> 
    [(t,FRPBase t j p (StateArrow s a) j p)] -> s -> FRPBase t i o a j [(t,p)]
withState manageThreads threads s = FRPBase $ raw $ statefulTransform lift $ StatefulArrow.withState (RSAGL.FRP.FRPBase.statefulForm manageThreads threads) s

-- | See 'RSAGL.StatefulArrow.withExposedState'
withExposedState :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) =>
    (forall x. j -> [(t,x)] -> [(t,(p,x))] -> [x]) -> 
    [(t,FRPBase t j p (StateArrow s a) j p)] -> FRPBase t i o a (j,s) ([(t,p)],s)
withExposedState manageThreads threads = statefulContext $ StatefulArrow.withExposedState $ RSAGL.FRP.FRPBase.statefulForm manageThreads threads

-- | The 'statefulContext' combinator allows a 'StatefulArrow' to be embedded in an 'FRPBase', with
-- the provision that the 'StatefulArrow' does not have access to the threading operators.
statefulContext :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                       StatefulArrow a j p -> FRPBase t i o a j p
statefulContext = FRPBase . raw . statefulTransform lift

-- | The 'FRPBase' arrow can be made to appear as a 'StatefulArrow' using statefulForm.
statefulForm :: (Arrow a,ArrowChoice a,ArrowApply a) => 
     (forall x. i -> [(t,x)] -> [(t,(o,x))] -> [x]) -> 
     [(t,FRPBase t i o a i o)] -> StatefulArrow a i [(t,o)]
statefulForm manageThreads = ThreadedArrow.statefulForm manageThreads . map (second $ statefulThread . fromFRPBase)

-- | The essential mechanism of the FRPBase arrow is the layering of a StatefulArrow over a ThreadedArrow,
-- allowing threads that are both implicitly and explicitly stateful.  \texttt{statefulThread} implements this.
-- In the event of an explicit switch, all implicit state is lost.
statefulThread :: (Arrow a,ArrowChoice a) => 
                  StatefulArrow (ThreadedArrow t i o a) i o -> ThreadedArrow t i o a i o
statefulThread (StatefulArrow sf) = 
    proc b -> do (c,sf') <- sf -< b
                 ThreadedArrow.switchTerminate -< (Just $ statefulThread sf',c)
