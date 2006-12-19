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

module DefaultKeymap
    (default_keymap)
    where

-- |
-- Ensure that keymap entries containing a string longer than 1 end with a return character ('\n').
--
filterKeymap :: [(String,String)] -> [(String,String)]
filterKeymap keymap = concatMap (\(x,y) -> if (length x > 1) 
		                           then [((unwords $ words x) ++ "\r",y),((unwords $ words x) ++ "\n",y)] 
					   else [(x,y)])
					   keymap

default_keymap :: [(String,String)]
default_keymap = filterKeymap
		 [("x","anachronid"),
		  ("a","androsynth"),
		  ("A","ascendant"),
		  ("c","caduceator"),
		  ("e","encephalon"),
		  ("g","goliath"),
		  ("h","hellion"),
		  ("k","kraken"),
		  ("m","myrmidon"),
		  ("p","perennial"),
		  ("r","reptilian"),
		  ("R","recreant"),
		  (".","reroll"),
		  ("b","barbarian"),
		  ("c","consular"),
		  ("e","engineer"),
		  ("a","forceadept"),
		  ("m","marine"),
		  ("n","ninja"),
		  ("p","pilot"),
		  ("P","privateer"),
		  ("s","scout"),
		  ("S","shepherd"),
		  ("t","thief"),
		  ("w","warrior"),
		  ("k","move-n"),
		  ("j","move-s"),
		  ("h","move-e"),
		  ("l","move-w"),
		  ("y","move-ne"),
		  ("u","move-nw"),
		  ("b","move-se"),
		  ("n","move-sw"),
		  ("#quit","quit")]
