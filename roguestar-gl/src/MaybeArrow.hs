{-# LANGUAGE Arrows, MultiParamTypeClasses, FlexibleInstances #-}

-- | An Arrow supporting failure.
-- 
-- Caveats:
--
-- 'MaybeArrow' is not necessarily an 'ArrowTransformer'.  If the underlying arrow does not support 'ArrowChoice', then 'MaybeArrow' is still useful,
-- but the 'lift' operation is not supported.
--
-- Failed 'MaybeArrow's still call all underlying side effecting operations, except those wrapped with 'lift'.
--
module MaybeArrow
    (MaybeArrow(..),
     maybeA,
     guardA,
     extract,
     liftJust,
     liftConst,
     liftJustConst,
     example)
    where

import Prelude hiding (id,(.))
import Control.Arrow
import Control.Arrow.Transformer
import Data.Maybe
import Control.Category

newtype MaybeArrow a b c = MaybeArrow { runMaybeArrow :: (a (Maybe b) (Maybe c)) }

instance (Category a,Arrow a) => Category (MaybeArrow a) where
    (.) (MaybeArrow y) (MaybeArrow x) = MaybeArrow $ arr splitIt >>> first x >>> arr combineIt >>> y
        where splitIt m = (m,m)
              combineIt (n,Just _) = n
              combineIt _ = Nothing
    id = MaybeArrow id

instance (Arrow a) => Arrow (MaybeArrow a) where
    arr f = MaybeArrow $ arr $ fmap f
    first (MaybeArrow x) = MaybeArrow $ arr splitMaybe >>> first x >>> arr combineMaybe
        where splitMaybe (Just (m,n)) = (Just m,Just n)
              splitMaybe Nothing = (Nothing,Nothing)
              combineMaybe (Just m,Just n) = Just (m,n)
              combineMaybe _ = Nothing

instance (Arrow a,ArrowChoice a) => ArrowTransformer MaybeArrow a where
    lift a = MaybeArrow $ arr (maybe (Right Nothing) Left) >>> ((a >>> arr Just) ||| arr id)

-- | Embed a raw 'Maybe' value into a computation.
maybeA :: (Arrow a) => MaybeArrow a (Maybe b) b
maybeA = MaybeArrow $ arr (fromMaybe Nothing)

-- | Arbitrarily fail a computation.
guardA :: (Arrow a) => MaybeArrow a Bool ()
guardA = maybeA <<< arr (\x -> if x then Just () else Nothing)

-- | Get an explicit Maybe from a computation instead of failing.  Inverse of 'maybeA'.
extract :: (Arrow a) => MaybeArrow a b c -> MaybeArrow a b (Maybe c)
extract (MaybeArrow actionA) = MaybeArrow $ actionA >>> arr Just

-- | Lift an action that always succeeds.
liftJust :: (Arrow a) => a (Maybe b) c -> MaybeArrow a b c
liftJust actionA = MaybeArrow $ actionA >>> arr Just

-- | Lift an action that accepts a constant input.
liftConst :: (Arrow a) => b -> a b (Maybe c) -> MaybeArrow a () c
liftConst k actionA = MaybeArrow $ actionA <<< (arr $ const k)

-- | Combine 'liftJust' and 'liftConst'.
liftJustConst :: (Arrow a) => b -> a b c -> MaybeArrow a () c
liftJustConst k actionA = MaybeArrow $ arr Just <<< actionA <<< (arr $ const k)

-- | Simple example that answers the sum of all four Integers, if they are all provided.
-- Try: runMaybeArrow (example 2 (Just 3)) $ Just (4,Just 5)
example :: Integer -> Maybe Integer -> MaybeArrow (->) (Integer,Maybe Integer) Integer
example a m_b = proc (c,m_d) ->
    do b <- maybeA -< m_b
       d <- maybeA -< m_d
       returnA -< sum [a,b,c,d]
