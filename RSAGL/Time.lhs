\section{RSAGL.Time}

RSAGL.Time provides a fixed-point (as opposed to a floating-point number) representation of time.
This is necessary because the Float and Double types are inadequate to precisely represent large
quantities of time.  Furthermore, it matters what unit (seconds, milliseconds, days) the time is
represented with, a potential source of errors.  RSAGL.Time is unitless.

RSAGL.Time provides several functions that allow the time to be accessed as Float or Double values,
but in a way that mitigates floating-point precision issues.  For example, cycleSeconds and secondsSince.

RSAGL.Time could (should) be generalized to a generic fixed-point number representation, with the time-specific code 
remaining in RSAGL.Time.

This time library is designed to support real-time animation.

\begin{code}
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

newtype Time = Time Integer deriving (Show)
\end{code}

\subsection{Getting the current time}

getTime gets the current, absolute time, using Haskell's standard time facilities.

\begin{code}
getTime :: IO Time
getTime = 
    do (TOD secs picos) <- getClockTime
       return $ Time $ (secs * scale_factor) 
                       + ((picos * scale_factor) `div` 1000000000000)
\end{code}

\subsection{Time as a cyclical value}

cycleSeconds divides the time into n-seconds-long cycles, and answers the fraction of the cycle at this moment.
It rises from 0 to 1, and then cuts back to 0 at the beginning of the next cycle.

\begin{code}
cycleSeconds :: (Integral a,Num b,Fractional b) => a -> IO b
cycleSeconds secs = 
    do (Time t) <- getTime
       return $ (toSeconds $ Time $ t `mod` (toInteger secs * scale_factor)) 
                / (fromIntegral secs)
\end{code}

\subsection{Getting elapsed time}

secondsSince answers the time elapsed since the specified Time, in seconds.

\begin{code}
secondsSince :: (Num b,Fractional b) => Time -> IO b
secondsSince old_time = liftM (toSeconds . (subtract old_time)) getTime
\end{code}

\subsection{Coercion to and from seconds}

Be aware that floating-point number types might not be able to precisely represent large quantities of time.

\begin{code}
toSeconds :: (Num a,Fractional a) => Time -> a
toSeconds (Time x) = fromInteger x / scale_factor

fromSeconds :: (Real a) => a -> Time
fromSeconds x = Time $ round $ toRational $ x * scale_factor
\end{code}

\begin{code}
scale_factor :: (Num a) => a
scale_factor = 1000000
\end{code}

\subsection{Instances}

RSAGL.Time implements several useful typeclasses in the Num hierarchy.
Note that when you use fromInteger, toRational, and so on you are working
with time in seconds.

\begin{code}
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

\end{code}