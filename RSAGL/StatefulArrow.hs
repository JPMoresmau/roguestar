{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.StatefulArrow
    (StatefulArrow(..),
     StatefulFunction,
     internally)
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
internally :: (Arrow a) => StateArrow s a i o -> s -> StatefulArrow a i o
internally sa s = StatefulArrow $
    proc i -> do (o,s') <- runState sa -< (i,s)
                 returnA -< (o,internally sa s')
