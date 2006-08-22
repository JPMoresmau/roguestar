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

module Translation
    (Language(..),
     translator,
     tr)
    where

data Language = English
	      deriving (Eq,Enum,Show)

-- |
-- If the string corresponds to a language for which a translation exists,
-- answers the language, otherwise Nothing.
--
-- Supported languages:
-- "en" - english
--
translator :: String -> Maybe Language
translator "en" = Just English
translator "english" = Just English
translator _ = Nothing

-- |
-- Performs a translation.
--
tr :: Language -> [String] -> String

tr English ["window-title"] = "RogueStar - OpenGL"

tr _ args = unwords args
