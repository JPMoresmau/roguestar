module RSAGL.Time
    (Time,
     getTime,
     secondsSince,
     toSeconds,
     fromSeconds,
     cycleSeconds)
    where

import System.Time
import Control.Monad
import Data.Ratio

newtype Time = Time Integer deriving (Show) -- a fixed point (as in numbers, not as in programming) representation of the time

-- |
-- Get the time since some fixed moment in seconds.
--
getTime :: IO Time
getTime = do (TOD secs picos) <- getClockTime
	     return $ Time $ (secs * scale_factor) + ((picos * scale_factor) `div` 1000000000000)

-- |
-- Answers a value that increases from 0 to 1 every n seconds, then snaps back to 0.
--
cycleSeconds :: (Integral a,Num b,Fractional b) => a -> IO b
cycleSeconds secs = do (Time t) <- getTime
                       return $ (toSeconds $ (Time $ t `mod` (toInteger secs * 1000))) / (fromIntegral secs)

-- |
-- Answers the time elapsed since the specified Time, in seconds.
--
secondsSince :: (Num b,Fractional b) => Time -> IO b
secondsSince old_time = liftM (toSeconds . (\x -> x - old_time)) getTime

-- |
-- Coerce the time to seconds.
--
toSeconds :: (Num a,Fractional a) => Time -> a
toSeconds (Time x) = fromInteger x / scale_factor

-- |
-- Coerce the time from seconds.
--
fromSeconds :: (Real a) => a -> Time
fromSeconds x = Time $ round $ toRational $ x * 1000

-- |
-- Fixed point factor.
--
scale_factor :: (Num a) => a
scale_factor = 1000

instance Num Time where
    (+) (Time x) (Time y) = Time $ x + y
    (-) (Time x) (Time y) = Time $ x - y
    (*) (Time x) (Time y) = Time $ (x * y) `div` scale_factor
    negate (Time x) = Time (-x)
    abs (Time x) = Time $ abs x
    signum (Time x) = Time $ signum x * scale_factor
    fromInteger x = Time $ x * scale_factor
    
instance Fractional Time where
    (/) (Time x) (Time y) = Time $ (x * scale_factor) `div` y
    recip (Time x) = Time $ scale_factor `div` x
    fromRational x = Time $ round $ x * scale_factor
    
{-# RULES
"realToFrac/Time->Float"    realToFrac   = toSeconds :: Time -> Float
"realToFrac/Time->Double"   realToFrac   = toSeconds :: Time -> Double
    #-}
    
instance Real Time where
    toRational (Time x) = x % scale_factor
    
instance Eq Time where
    (==) (Time x) (Time y) = x == y
    
instance Ord Time where
    compare (Time x) (Time y) = compare x y