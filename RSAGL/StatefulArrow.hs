{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.StatefulArrow
    (StatefulArrow(..),
     StatefulFunction,
     stateContext,
     runStateMachine)
    where

import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Arrow.Operations
import Control.Arrow.Transformer

-- |
-- A StatefulArrow indicates the form it will take on its next iteration.
--
data StatefulArrow a i o = StatefulArrow { runStatefulArrow :: (a i (o,StatefulArrow a i o)) }

instance (Arrow a) => Arrow (StatefulArrow a) where
    (>>>) (StatefulArrow sf1) (StatefulArrow sf2) = StatefulArrow $
        proc a -> do (b,sf1') <- sf1 -< a
                     (c,sf2') <- sf2 -< b
                     returnA -< (c,sf1' >>> sf2')
    arr = lift . arr
    first (StatefulArrow sf) = StatefulArrow $
        proc (b,d) -> do (c,sf') <- sf -< b
                         returnA -< ((c,d),first sf')

type StatefulFunction = StatefulArrow (->)

instance (Arrow a) => ArrowTransformer StatefulArrow a where
    lift f = StatefulArrow $ f &&& (arr $ const $ lift f)

-- |
-- "Hide" a StateArrow inside a StatefulArrow.
-- The StateArrow's internal state is fed back to it on each iteration.
-- 
stateContext :: (Arrow a) => StateArrow s a i o -> s -> StatefulArrow a i o
stateContext sa s = StatefulArrow $
    proc i -> do (o,s') <- runState sa -< (i,s)
                 returnA -< (o,stateContext sa s')

runStateMachine :: (ArrowChoice a,ArrowApply a) => StatefulArrow a b b -> a [b] [b]
runStateMachine stateful_arrow = 
    proc x -> do runStateMachine_ -< (([],stateful_arrow),x)

runStateMachine_ :: (ArrowChoice a,ArrowApply a) => a (([b],StatefulArrow a b b),[b]) [b]
runStateMachine_ =
    proc ((reversed_so_far,StatefulArrow stateful),x) -> 
        do case x of
                  [] -> returnA -< reverse reversed_so_far
                  (i:is) -> do (o,stateful') <- app -< (stateful,i)
                               runStateMachine_ -< ((o:reversed_so_far,stateful'),is)