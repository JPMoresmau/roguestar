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

module Seconds
    (Seconds,
     seconds,
     secondsSince,
     cycleSeconds)
    where

import System.Time
import Data.Ratio
import Control.Monad

type Seconds = Rational

-- |
-- The time since some fixed moment in seconds.
--
seconds :: IO Seconds
seconds = do (TOD secs picos) <- getClockTime
	     return $ (secs % 1) + (picos % 1000000000000)

-- |
-- The time since the specified moment in seconds.
--
secondsSince :: (Fractional n) => Seconds -> IO n
secondsSince moment = do secs <- seconds
                         return $ (\x -> fromRational $ x - moment) secs

--liftM (fromRational . (- moment)) seconds

-- |
-- Answers a value between in the rand [0..1) indicating how far we are through
-- an n-second cycle.  If you multiplied the result by 2*pi and used that number
-- to rotate an object, the object would spin once every n seconds.
--
cycleSeconds :: (Fractional n) => Integer -> IO n
cycleSeconds cycle_length = do (TOD secs picos) <- getClockTime
			       return $ (fromInteger (secs `mod` cycle_length) + (fromInteger picos / 1000000000000)) / fromInteger cycle_length
