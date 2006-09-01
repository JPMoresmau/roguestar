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
import Data.Maybe

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

stat_quantity_to_quality_table :: [(String,String)]
stat_quantity_to_quality_table = concatMap (\(x,y) -> map (\x' -> (show x',y)) x)
    [([(-1)..1],"average"),
     ([2..4],"above-average"),
     ([5..8],"good"),
     ([9..13],"excelent"),
     ([14..19],"incredible"),
     ([(-2)..(-4)],"below-average"),
     ([(-5)..(-8)],"poor")]

statQuantityToQuality :: String -> String
statQuantityToQuality value | value `elem` (map fst stat_quantity_to_quality_table) =
				fromJust $ lookup value stat_quantity_to_quality_table
statQuantityToQuality value | head value == '-' = "pathetic"
statQuantityToQuality _ = "astonishing"

-- |
-- Performs a translation.
--
translateStr :: Language -> [String] -> String

-- 
-- General redirection of translateStr requests that probably apply to all languages.
--
translateStr language ["menu-item","select-race",species] = translateStr language ["species",species]
translateStr language ["table-data",_,"gender",gender] = translateStr language ["gender",gender]
translateStr language ["table-data",_,stat_name,stat_value] | stat_name `elem` ["str","dex","con","int","per","cha","mind"]
								= translateStr language ["statistic",stat_value]
translateStr language ["statistic",stat_value] = translateStr language ["statistic-qualitative",statQuantityToQuality stat_value] ++ " (" ++ stat_value ++ ")"

--
-- English language translation.
--
translateStr English ["window-title"] = "RogueStar - OpenGL"

translateStr English ["menu-title","select-race"] = "Select a species for your starting character:"

translateStr English ["table",_,"str"] =      "      Strength: "
translateStr English ["table",_,"dex"] =      "     Dexterity: "
translateStr English ["table",_,"con"] =      "  Constitution: "
translateStr English ["table",_,"int"] =      "  Intelligence: "
translateStr English ["table",_,"per"] =      "    Perception: "
translateStr English ["table",_,"cha"] =      "      Charisma: "
translateStr English ["table",_,"mind"] =     "   Mindfulness: "
translateStr English ["table",_,"gender"] =   "        Gender: "
translateStr English ["table",_,"hp/maxhp"] = "    Hit Points: "
translateStr English ["table",_,"hp"] =       "    Hit Points: "
translateStr English ["table",_,"maxhp"] =    "Max Hit Points: "

translateStr English ["statistic-qualitative",qualitative] = qualitative

translateStr English ["gender",gender] = titleCapitalize gender

translateStr English ["species",species] = titleCapitalize species

translateStr English ["user-selected-species",species] = "Welcome, young " ++ species ++ "."

translateStr _ args = strToUpper $ unwords args
