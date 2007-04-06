\section{RSAGL.SwitchedArrow}

A SwitchedArrow is very much like a StatefulArrow, in that it can specify a new form of itself
to be evaluated on each iteration.

Unlike a StatefulArrow, a Switched Arrow retains type information about itself
allowing any of it's delegate arrows to explicitly switch (and thereby terminate 
execution of the current iteration of) the switched arrow.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.SwitchedArrow
    (SwitchedArrow,
     SwitchedFunction,
     switchContinue,
     switchTerminate,
     statefulForm,
     RSAGL.SwitchedArrow.withState,
     RSAGL.SwitchedArrow.withExposedState)
    where

import RSAGL.StatefulArrow as StatefulArrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Error
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer
import Control.Arrow

type SwitchedFunction i o j p = SwitchedArrow i o (->) j p
\end{code}

The SwitchedArrow is really a special case of the ErrorArrow with ArrowApply.

\begin{code}
newtype SwitchedArrow i o a j p = SwitchedArrow (ErrorArrow (o,SwitchedArrow i o a i o) a j p)

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
\end{code}

\subsection{Switching operators}
\label{switchContinue}
\label{switchTerminate}

A switchable arrow has two switching operators: switchContinue and switchTerminate.

switchContinue is just like calling app (arrow apply), except that the new switch 
replaces the current switch the next time the arrow is executed.  Because this is a
continuation, we pass the input of the switch into the new switch.

switchTerminate ends execution of the current switch.  Therefore, it requires the
output of the switch which will be delivered immediately back to the caller.  Like
switchContinue, the new switch replaces the current switch the next time the arrow is
executed.

\begin{code}
switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) => SwitchedArrow i o a (SwitchedArrow i o a i o,i) o
switchContinue =
    proc (switch,i) -> do o <- app -< (switch,i)
                          SwitchedArrow raise -< (o,switch)
                          returnA -< o

switchTerminate :: (Arrow a,ArrowChoice a) => SwitchedArrow i o a (SwitchedArrow i o a i o,o) o
switchTerminate = 
    proc (switch,o) -> do SwitchedArrow raise -< (o,switch)
                          returnA -< o
\end{code}

\subsection{The StatefulArrow form of a SwitchedArrow}
\label{statefulForm}

StatefulArrows and SwitchedArrows can both be thought of as automata or self-modifying programs.
statefulForm allows a SwitchedArrow to appear to the outside world as a StatefulArrow.

\begin{code}
statefulForm :: (Arrow a,ArrowChoice a) => SwitchedArrow i o a i o -> StatefulArrow a i o
statefulForm (SwitchedArrow a) = StatefulArrow $ runError (switch) handler
    where handler = proc (_,(o,newswitch)) -> do returnA -< (o,statefulForm newswitch)
          switch = proc i -> do o <- a -< i
                                returnA -< (o,statefulForm $ SwitchedArrow a)
\end{code}

\subsection{Mixing SwitchedArrows and StateArrows}

These are the StateArrow-related combinators introduced with StatefulArrow
\footnote{Page \pageref{withState}}

\begin{code}
withState :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                SwitchedArrow i o (StateArrow s a) i o -> (i -> s) -> StatefulArrow a i o
withState switch2 f = StatefulArrow.withState (statefulForm switch1) undefined
    where switch1 = proc i ->
              do lift store -< f i
                 RSAGL.SwitchedArrow.switchContinue -< (switch2,i)

withExposedState :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                        SwitchedArrow i o (StateArrow s a) i o -> StatefulArrow a (i,s) (o,s)
withExposedState switch = StatefulArrow.withExposedState (statefulForm switch)
\end{code}