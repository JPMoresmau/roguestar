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

module String
    (strToLower,
     strToUpper,
     capitalize,
     titleCapitalize,
     padLeft,
     padRight)
    where

import Data.Char

strToLower :: String -> String
strToLower = map toLower

strToUpper :: String -> String
strToUpper = map toUpper

capitalize :: String -> String
capitalize "" = ""
capitalize str = let str' = unwords $ words str
		     in (toUpper $ head str') : (tail str')

titleCapitalize :: String -> String
titleCapitalize str = capitalize $ unwords $ map capitalizeTitleWord $ words str
    where capitalizeTitleWord "the" = "the"
	  capitalizeTitleWord "a" = "a"
	  capitalizeTitleWord "an" = "an"
	  capitalizeTitleWord x = capitalize x

padLeft :: Int -> String -> String
padLeft n str = replicate (max 1 $ n - length str) ' ' ++ str

padRight :: Int -> String -> String
padRight n str = str ++ replicate (max 1 $ n - length str) ' '