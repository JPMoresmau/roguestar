--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module Time
    (Time,
     getTime,
     secondsSince,
     toSeconds,
     fromSeconds,
     cycleSeconds)
    where

import System.Time
import Control.Monad

data Time = Time Integer deriving (Show) -- in milliseconds

-- |
-- Get the time since some fixed moment in seconds.
--
getTime :: IO Time
getTime = do (TOD secs picos) <- getClockTime
	     return $ Time $ (secs * scale_factor) + ((picos * scale_factor) `div` 1000000000000)

-- |
-- Answers a value that increases from 0 to 1 every n seconds.
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
-- The time in seconds.
--
toSeconds :: (Num a,Fractional a) => Time -> a
toSeconds (Time x) = fromInteger x / scale_factor

fromSeconds :: (Real a) => a -> Time
fromSeconds x = Time $ round $ toRational $ x * 1000

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
    
instance Eq Time where
    (==) (Time x) (Time y) = x == y
    
instance Ord Time where
    compare (Time x) (Time y) = compare x y