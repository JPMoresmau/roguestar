\section{Managing Expensive Arrows}

The \texttt{ArrowTransformerOptimizer} optimizes arrow transformers that have expensive binding operations by intelligently extracting all lifted computations
and representing pure computations as lifted pure computations.

\begin{code}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module RSAGL.ArrowTransformerOptimizer
    (ArrowTransformerOptimizer,
     raw,
     collapseArrowTransformer)
    where

import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow
import Control.Arrow.Transformer

data ArrowTransformerOptimizer a l b c where
    Raw :: a l b c -> ArrowTransformerOptimizer a l b c
    Lifted :: (ArrowTransformer a l) => l b c -> ArrowTransformerOptimizer a l b c
    Joined :: (ArrowTransformer a l) => l b x -> a l x y -> l y c -> ArrowTransformerOptimizer a l b c

instance (Category (a l), Category l,ArrowTransformer a l) => Category (ArrowTransformerOptimizer a l) where
    (Raw a) . (Raw b) = Raw (a . b)
    (Lifted a) . (Lifted b) = Lifted (a . b)
    (Raw a) . (Lifted b) = Joined b a (arr id)
    (Lifted a) . (Raw b) = Joined (arr id) b a
    (Lifted l) . (Joined x y z) = Joined x y (l . z)
    (Joined x y z) . (Lifted l) = Joined (x . l) y z
    (Joined a b c) . (Joined x y z) = Joined x (b . lift (a . z) . y) c
    (Joined x y z) . (Raw b) = Joined (arr id) (y . lift x . b) z
    (Raw a) . (Joined x y z) = Joined x (a . lift z . y) (arr id)
    id = Lifted (arr id)

instance (ArrowTransformer a l) => Arrow (ArrowTransformerOptimizer a l) where
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
collapseArrowTransformer (Joined x y z) = lift z . y . lift x
\end{code}

