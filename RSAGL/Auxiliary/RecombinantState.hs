{-# LANGUAGE TypeFamilies #-}
module RSAGL.Auxiliary.RecombinantState
    (RecombinantState(..))
    where

-- | Describes concurrency-aware state.  The goal is to take some stateful information, clone it into a variety of concurrent threads,
-- and then recombine with the (possibly modified) state as the concurrent threads complete.
class RecombinantState s where
    type SubState s :: *
    clone :: s -> SubState s
    recombine :: s -> SubState s -> s

instance RecombinantState () where
    type SubState () = ()
    clone = id
    recombine = const
