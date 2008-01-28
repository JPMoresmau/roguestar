\section{Managing Expensive Arrows}

The \texttt{ArrowTransformerShield} optimizes arrow transformers that have expensive binding operations by intelligently extracting all lifted operations.

\begin{code}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module RSAGL.ArrowTransformerShield
    (ArrowTransformerShield,
     raw,
     collapseATS)
    where

import Control.Arrow
import Control.Arrow.Transformer

data ArrowTransformerShield a l b c where
    Raw :: a l b c -> ArrowTransformerShield a l b c
    Lifted :: (ArrowTransformer a l) => l b c -> ArrowTransformerShield a l b c
    Joined :: (ArrowTransformer a l) => l b x -> ArrowTransformerShield a l x y -> l y c -> ArrowTransformerShield a l b c

instance (ArrowTransformer a l) => Arrow (ArrowTransformerShield a l) where
    (Raw a) >>> (Raw b) = Raw (a >>> b)
    (Lifted a) >>> (Lifted b) = Lifted (a >>> b)
    (Raw a) >>> (Lifted b) = Joined (arr id) (Raw a) b
    (Lifted a) >>> (Raw b) = Joined a (Raw b) (arr id)
    (Lifted l) >>> (Joined x y z) = Joined (l >>> x) y z
    (Joined x y z) >>> (Lifted l) = Joined x y (z >>> l)
    (Joined a b c) >>> (Joined x y z) = Joined a (b >>> sandwichLift (c >>> x) >>> y) z
    (Joined x y z) >>> b = Joined x (y >>> sandwichLift z >>> b) (arr id)
    a >>> (Joined x y z) = Joined (arr id) (a >>> sandwichLift x >>> y) z
    first (Raw a) = Raw (first a)
    first (Lifted a) = Lifted (first a)
    first (Joined x y z) = Raw $ first $ collapseATS (Joined x y z)
    arr a = Lifted (arr a)

instance (ArrowTransformer a l) => ArrowTransformer (ArrowTransformerShield a) l where
    lift = Lifted

raw :: (ArrowTransformer a l) => a l b c -> ArrowTransformerShield a l b c
raw = Raw

sandwichLift :: (ArrowTransformer a l) => l b c -> ArrowTransformerShield a l b c
sandwichLift = Raw . lift

collapseATS :: ArrowTransformerShield a l b c -> a l b c
collapseATS (Raw a) = a
collapseATS (Lifted a) = lift a
collapseATS (Joined x y z) = lift x >>> collapseATS y >>> lift z
\end{code}

