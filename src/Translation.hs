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
     stringToLanguage,
     translateStr)
    where

import String

data Language = English
	      deriving (Eq,Enum,Show)

-- |
-- If the string corresponds to a language for which a translation exists,
-- answers the language, otherwise Nothing.
--
-- Supported languages:
-- "en" - english
--
stringToLanguage :: String -> Maybe Language
stringToLanguage "en" = Just English
stringToLanguage "english" = Just English
stringToLanguage _ = Nothing

-- |
-- Performs a translation.
--
translateStr :: Language -> [String] -> String

-- 
-- General redirection of translateStr requests that probably apply to all languages.
--
translateStr language ["menu-item","select-race",species] = translateStr language ["species",species]


--
-- English language translation.
--
translateStr English ["window-title"] = "RogueStar - OpenGL"

translateStr English ["menu-title","select-race"] = "Select a species for your starting character:"

translateStr English ["species",species] = titleCapitalize species

translateStr English ["user-selected-species",species] = titleCapitalize species

translateStr _ args = strToUpper $ unwords args
