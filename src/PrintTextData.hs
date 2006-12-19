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

module PrintTextData
    (PrintTextMode(..),
     TextType(..))
    where

-- |
-- How to run printText.
--
-- Limited -- print only the most recent lines at the top of the screen
-- Unlimited -- fill the entire screen with text
-- Disabled -- don't print any text at all
--
data PrintTextMode = Limited -- print only a few lines (or only 1)
		   | Unlimited -- fill the entire screen with text
		   | Disabled -- don't print any text at all
		     deriving (Eq)

-- |
-- Type of any line of text printed on the screen.  We print the
-- different TextTypes in different colors for readability.
--
-- Untranslated -- we didn't recognize some input from roguestar-engine, so we print it in the raw
-- Information -- information from roguestar-engine
-- UserQuery -- a question put to the user
-- GUIMessage -- printed from the GUI, not the engine.
--
data TextType = Untranslated
	      | Information
	      | UserQuery
	      | GUIMessage