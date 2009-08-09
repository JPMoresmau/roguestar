\section{RSAGL.Time}

RSAGL.Time provides a fixed-point (as opposed to a floating-point number) representation of time.
This is necessary because the Float and Double types are inadequate to precisely represent large
quantities of time.

This time library is designed to support real-time animation.

\begin{code}
{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module RSAGL.FRP.Time
    (Time,
     Rate,
     Acceleration,
     Frequency,
     fps30,
     fps60,
     fps120,
     minute,
     day,
     month,
     year,
     fromSeconds,
     toSeconds,
     getTime,
     cyclical,
     cyclical',
     over,
     rate,
     time,
     perSecond,
     per,
     interval,
     withTime)
    where

import RSAGL.Math.AbstractVector
import System.Time
import Control.Monad
import RSAGL.Math.Affine

{-# INLINE time_resolution #-}
time_resolution :: (Num n) => n
time_resolution = 1000000

newtype Time = Time Integer deriving (Show,Eq,Ord)
newtype Rate a = Rate a deriving (Show,Eq,Ord,AffineTransformable)
type Acceleration a = Rate (Rate a)
type Frequency = Rate Double

instance AbstractZero Time where
    zero = Time 0

instance AbstractAdd Time Time where
    add (Time a) (Time b) = Time $ a + b

instance AbstractSubtract Time Time where
    sub (Time a) (Time b) = Time $ a - b

instance AbstractScale Time where
    scalarMultiply d (Time t) = Time $ round $ d * fromInteger t

instance AbstractVector Time

instance (AbstractZero a) => AbstractZero (Rate a) where
    zero = Rate zero

instance (AbstractAdd a a) => AbstractAdd (Rate a) (Rate a) where
    add (Rate a) (Rate b) = Rate $ a `add` b

instance (AbstractSubtract a a) => AbstractSubtract (Rate a) (Rate a) where
    sub (Rate a) (Rate b) = Rate $ a `sub` b

instance (AbstractScale a) => AbstractScale (Rate a) where
    scalarMultiply d (Rate r) = Rate $ scalarMultiply d r

instance (AbstractVector a) => AbstractVector (Rate a)
\end{code}

\subsection{Getting and Constructing Time}

getTime gets the current, absolute time, using Haskell's standard time facilities, getClockTime.

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

fps30 :: Frequency
fps30 = perSecond 30

fps60 :: Frequency
fps60 = perSecond 60

fps120 :: Frequency
fps120 = perSecond 120

fromSeconds :: Double -> Time
fromSeconds = Time . round . (* time_resolution)

toSeconds :: Time -> Double
toSeconds (Time t) = fromInteger t / time_resolution

getTime :: IO Time
getTime = 
    do (TOD secs picos) <- getClockTime
       return $ Time $ secs * time_resolution + (picos * time_resolution) `div` 1000000000000
\end{code}

\subsection{Modulo Division for Time}

\texttt{cyclical} answers the amount of time into a cycle.  \texttt{cyclical'} answers the fraction of time into a cycle, 
in the range \texttt{0 <= x <= 1}.

\begin{code}
cyclical :: Time -> Time -> Time
cyclical (Time t) (Time k) = Time $ t `mod` k

cyclical' :: Time -> Time -> Double
cyclical' t k = (toSeconds $ t `cyclical` k) / toSeconds k
\end{code}

\subsection{Rate as Change over Time}

\begin{code}
{-# INLINE over #-}
over :: (AbstractVector a) => Rate a -> Time -> a
over (Rate a) (Time t) = (fromInteger t / time_resolution) `scalarMultiply` a

{-# INLINE rate #-}
rate :: (AbstractVector a) => (a,Time) -> (a,Time) -> Rate a
rate (x,t1) (y,t2) = (y `sub` x) `per` (t2 `sub` t1)

perSecond :: a -> Rate a
perSecond a = Rate a

{-# INLINE per #-}
per :: (AbstractVector a) => a -> Time -> Rate a
per a (Time t) = Rate $ (recip $ fromInteger t / time_resolution) `scalarMultiply` a

interval :: Frequency -> Time
interval (Rate x) = fromSeconds $ recip x

time :: Double -> Rate Double -> Time
time d r = interval $ withTime (fromSeconds 1) (/d) r

{-# INLINE withTime #-}
withTime :: (AbstractVector a,AbstractVector b) => Time -> (a -> b) -> Rate a -> Rate b
withTime t f = (`per` t) . f . (`over` t)
\end{code}
