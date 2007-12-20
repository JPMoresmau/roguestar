\section{RSAGL.Time}

RSAGL.Time provides a fixed-point (as opposed to a floating-point number) representation of time.
This is necessary because the Float and Double types are inadequate to precisely represent large
quantities of time.

This time library is designed to support real-time animation.

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}

module RSAGL.Time
    (Time,
     Rate,
     minute,
     day,
     month,
     year,
     fromSeconds,
     toSeconds,
     getTime,
     over,
     rate,
     perSecond,
     per)
    where

import RSAGL.AbstractVector
import System.Time
import Control.Monad
import Data.Fixed
import Data.Ratio

newtype Time = Time Pico deriving (Show,Eq,Ord,AbstractVector)
newtype Rate a = Rate a deriving (Show,Eq,Ord,AbstractVector)
\end{code}

\subsection{Getting and Constructing Time}

getTime gets the current, absolute time, using Haskell's standard time facilities.

\begin{code}
minute :: Time
minute = fromSeconds 60

hour :: Time
hour = scalarMultiply 60 minute

day :: Time
day = scalarMultiply 24 hour

month :: Time
month = scalarMultiply 30.43 day

year :: Time
year = scalarMultiply 365.25 month

fromSeconds :: Double -> Time
fromSeconds = Time . realToFrac

toSeconds :: Time -> Double
toSeconds (Time t) = realToFrac t

getTime :: IO Time
getTime = 
    do (TOD secs picos) <- getClockTime
       return $ Time $ fromIntegral secs + fromRational (picos%(resolution (undefined :: E12)))
\end{code}

\subsection{Rate as Change over Time}

\begin{code}
over :: (AbstractVector a) => Rate a -> Time -> a
over (Rate a) (Time t) = realToFrac t `scalarMultiply` a

rate :: (AbstractVector a) => (a,Time) -> (a,Time) -> Rate a
rate (x,t1) (y,t2) = (y `sub` x) `per` (t2 `sub` t1)

perSecond :: a -> Rate a
perSecond a = Rate a

per :: (AbstractVector a) => a -> Time -> Rate a
per a (Time t) = Rate $ realToFrac (recip t) `scalarMultiply` a
\end{code}