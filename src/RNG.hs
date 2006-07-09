--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
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

-- |
-- Why do we need our own random number generation facilities?  Simply
-- because we can't guarantee that the system's random number generator
-- will be identical from one version of the system to the next, and we
-- certainly can't guarantee that between different implementations of
-- the system.  Having our own RNG facility means that we can write
-- a seed value to a save file and generate the same psuedo-random
-- sequence in a completely different environment.  Thus, a map may be
-- defined entirely by its psuedo-random seed.

module RNG
    (randomIntegerStream,
     randomIntegerStreamStream,
     randomIntegerLine,
     randomIntegerGrid)
    where

import Data.List
import ListUtils

-- |
-- Generates the next in a sequence of psuedo-random Integers.
-- These numbers should not be used raw.  (Due to Insufficient 
-- "random-ness" of the least significant bit.)  Use a 
-- randomIntegerStream[Stream].
--
nextRandomSeed :: Integer -> Integer
nextRandomSeed x = (x * 0x5DEECE66D + 0xB) `mod` (2^48)

-- |
-- A stream of random integers from a seed.
--
randomIntegerStream :: Integer -> [Integer]
randomIntegerStream x = let nri = nextRandomSeed x
                            in (nri `quot` 24) : (randomIntegerStream nri)

-- |
-- A stream of random integer streams.  Good when you need to do
-- a lot of splitting.
--
randomIntegerStreamStream :: Integer -> [[Integer]]
randomIntegerStreamStream x = let nri1 = nextRandomSeed x
                                  nri2 = nextRandomSeed nri1
                                  in (randomIntegerStream (nri1 + 1) :
                                      (randomIntegerStreamStream (nri2 - 1)))

-- |
-- An infinite (in both directions) sequence of random Integers, based
-- on a seed.
--
randomIntegerLine :: Integer -> (Integer -> Integer)
randomIntegerLine seed = bidirectionalAccessor1D $ randomIntegerStream seed

-- |
-- An infinite (in all directions) grid of random Integers, based
-- on a seed.
--
randomIntegerGrid :: Integer -> ((Integer,Integer) -> Integer)
randomIntegerGrid seed = bidirectionalAccessor2D $ map randomIntegerStream $ randomIntegerStream seed
