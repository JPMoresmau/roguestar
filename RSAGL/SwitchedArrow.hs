{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.SwitchedArrow
    (SwitchedArrow,
     Switch,
     SwitchedFunction,
     ArrowSwitched(..),
     switchedContext)
    where

import RSAGL.StatefulArrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Error
import Control.Arrow.Transformer
import Control.Arrow

type Switch a i o = SwitchedArrow i o a i o

type SwitchedFunction i o j p = SwitchedArrow i o (->) j p

-- |
-- A SwitchedArrow is very much like a StatefulArrow, in that it can specify a new form of itself
-- to be evaluated on each iteration.
--
-- Unlike a StatefulArrow, a Switched Arrow retains type information about itself
-- allowing any of it's delegate arrows to explicitly switch (and thereby terminate 
-- execution of the current iteration of) the switched arrow.
--
-- This allows extra flexability when it comes to constructing state machines.
--

newtype SwitchedArrow i o a j p = SwitchedArrow (ErrorArrow (o,Switch a i o) a j p)

instance (Arrow a,ArrowChoice a) => Arrow (SwitchedArrow i o a) where
    (>>>) (SwitchedArrow sa1) (SwitchedArrow sa2) = SwitchedArrow $ sa1 >>> sa2
    arr = SwitchedArrow . arr
    first (SwitchedArrow a) = SwitchedArrow $ first a

instance (Arrow a,ArrowChoice a) => ArrowTransformer (SwitchedArrow i o) a where
    lift = SwitchedArrow . lift

instance (ArrowChoice a) => ArrowChoice (SwitchedArrow i o a) where
    left (SwitchedArrow a) = SwitchedArrow $ left a

instance (ArrowChoice a,ArrowApply a) => ArrowApply (SwitchedArrow i o a) where
    app = SwitchedArrow $ proc (SwitchedArrow a,b) -> do app -< (a,b)

class ArrowSwitched as where
    switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) => as i o a (as i o a i o,i) o
    switchTerminate :: (Arrow a,ArrowChoice a) => as i o a (as i o a i o,o) o

instance ArrowSwitched SwitchedArrow where
    switchContinue = switchContinuePrim
    switchTerminate = switchTerminatePrim

switchContinuePrim :: (Arrow a,ArrowChoice a,ArrowApply a) => SwitchedArrow i o a (Switch a i o,i) o
switchContinuePrim =
    proc (switch,i) -> do o <- app -< (switch,i)
                          SwitchedArrow raise -< (o,switch)
                          returnA -< o

switchTerminatePrim :: (Arrow a,ArrowChoice a) => SwitchedArrow i o a (Switch a i o,o) o
switchTerminatePrim = 
    proc (switch,o) -> do SwitchedArrow raise -< (o,switch)
                          returnA -< o

switchedContext :: (Arrow a,ArrowChoice a) => Switch a i o -> StatefulArrow a i o
switchedContext (SwitchedArrow a) = StatefulArrow $ runError (switch) handler
    where handler = proc (_,(o,newswitch)) -> do returnA -< (o,switchedContext newswitch)
          switch = proc i -> do o <- a -< i
                                returnA -< (o,switchedContext $ SwitchedArrow a)