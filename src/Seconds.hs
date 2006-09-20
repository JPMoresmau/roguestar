module Seconds
    (seconds,
     cycleSeconds)
    where

import System.Time
import Data.Ratio

-- |
-- The time since some fixed moment in seconds.
--
seconds :: IO Rational
seconds = do (TOD secs picos) <- getClockTime
	     return $ (secs % 1) + (picos % 1000000000000)

-- |
-- Answers a value between in the rand [0..1) indicating how far we are through
-- an n-second cycle.  If you multiplied the result by 2*pi and used that number
-- to rotate an object, the object would spin once every n seconds.
--
cycleSeconds :: (Fractional n) => Integer -> IO n
cycleSeconds cycle_length = do (TOD secs picos) <- getClockTime
			       return $ (fromInteger (secs `mod` cycle_length) + (fromInteger picos / 1000000000000)) / fromInteger cycle_length
