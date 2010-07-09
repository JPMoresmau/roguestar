{-# LANGUAGE TypeFamilies #-}
module RSAGL.FRP.RecombinantState
    (RecombinantState(..))
    where

-- | Describes concurrency-aware state.  The goal is to take some stateful
-- information, clone it into a variety of concurrent threads, and then
-- recombine using the (possibly modified) state.
class RecombinantState s where
    type SubState s :: *
    -- | A new version of the state, which should carry the context,
    -- but not the content, of the original.  I.e., the original
    -- content will be re-merged during the recombination phase.
    clone :: s -> SubState s
    -- | Recombine the modified, cloned information with the
    -- original state.
    recombine :: s -> SubState s -> s

instance RecombinantState () where
    type SubState () = ()
    clone = id
    recombine = const

