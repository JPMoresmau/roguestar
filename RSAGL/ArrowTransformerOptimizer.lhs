\section{Managing Expensive Arrows}

The \texttt{ArrowTransformerOptimizer} optimizes arrow transformers that have expensive binding operations by intelligently extracting all lifted computations
and representing pure computations as lifted pure computations.

\begin{code}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module RSAGL.ArrowTransformerOptimizer
    (ArrowTransformerOptimizer,
     raw,
     collapseArrowTransformer)
    where

import Control.Arrow
import Control.Arrow.Transformer

data ArrowTransformerOptimizer a l b c where
    Raw :: a l b c -> ArrowTransformerOptimizer a l b c
    Lifted :: (ArrowTransformer a l) => l b c -> ArrowTransformerOptimizer a l b c
    Joined :: (ArrowTransformer a l) => l b x -> a l x y -> l y c -> ArrowTransformerOptimizer a l b c

instance (ArrowTransformer a l) => Arrow (ArrowTransformerOptimizer a l) where
    (Raw a) >>> (Raw b) = Raw (a >>> b)
    (Lifted a) >>> (Lifted b) = Lifted (a >>> b)
    (Raw a) >>> (Lifted b) = Joined (arr id) a b
    (Lifted a) >>> (Raw b) = Joined a b (arr id)
    (Lifted l) >>> (Joined x y z) = Joined (l >>> x) y z
    (Joined x y z) >>> (Lifted l) = Joined x y (z >>> l)
    (Joined a b c) >>> (Joined x y z) = Joined a (b >>> lift (c >>> x) >>> y) z
    (Joined x y z) >>> (Raw b) = Joined x (y >>> lift z >>> b) (arr id)
    (Raw a) >>> (Joined x y z) = Joined (arr id) (a >>> lift x >>> y) z
    first (Raw a) = Raw (first a)
    first (Lifted a) = Lifted (first a)
    first (Joined x y z) = Joined (first x) (first y) (first z)
    arr a = Lifted (arr a)

instance (ArrowTransformer a l) => ArrowTransformer (ArrowTransformerOptimizer a) l where
    lift = Lifted

raw :: (ArrowTransformer a l) => a l b c -> ArrowTransformerOptimizer a l b c
raw = Raw

collapseArrowTransformer :: ArrowTransformerOptimizer a l b c -> a l b c
collapseArrowTransformer (Raw a) = a
collapseArrowTransformer (Lifted a) = lift a
collapseArrowTransformer (Joined x y z) = lift x >>> y >>> lift z
\end{code}

