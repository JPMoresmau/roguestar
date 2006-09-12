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
import WordGenerator
import System.Random
import Data.List

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
     ([14..19],"superior"),
     ([(-4)..(-2)],"below-average"),
     ([(-8)..(-5)],"poor")]

statQuantityToQuality :: String -> String
statQuantityToQuality value | value `elem` (map fst stat_quantity_to_quality_table) =
				fromJust $ lookup value stat_quantity_to_quality_table
statQuantityToQuality value | head value == '-' = "pathetic"
statQuantityToQuality _ = "astonishing"

generateName :: String -> String -> String
generateName entity_name = maybe 
			   (\_ -> "?") 
			   (\gen -> \x -> generateWord gen $mkStdGen (read x :: Int)) 
			   $ lookup entity_name entity_name_generators

entity_name_generators :: [(String,WordGenerator)]
entity_name_generators = 
    [
     ("anachronid",WordGenerator {
				  softs=["f","g","h","j","l","m","n","r","s","v","w","z"],
				  hards=["k","q","t","x"],
				  word_patterns=[[Left Hard,Left Hard,Left Hard,Left Soft,Left Hard],
						 [Left Hard,Left Hard,Left Hard,Left Hard,Left Soft,Left Hard]]
				 }),
     ("androsynth",WordGenerator {
				  softs=["1","2","3","4","5","6","7","8","9","0"],
				  hards=["N","X","K"],
				  word_patterns=[[Left Hard,Left Soft,Left Soft,Left Soft,Right "-",Left Soft]]
				 }),
     ("ascendant",WordGenerator {
				 softs=["ae","ia","ea","ou"],
				 hards=["nil","rak","hyh","l","k","ch","sh","n"],
				 word_patterns=[[Left Hard,Left Soft,Left Hard],
						[Left Hard,Left Soft,Left Hard,Left Soft,Left Hard],
						[Left Hard,Left Soft,Left Hard,Left Soft,Left Hard,Left Soft,Left Hard]]
				}),
     ("caduceator",WordGenerator {
				  softs=["a","e","i","o","u","y"],
				  hards=["s","sh","ssh","sssh","hs","hss","hsss","ss","sss","h"],
				  word_patterns=[[Left Soft,Left Hard,Left Soft,Left Hard],
						 [Left Hard,Left Soft,Left Hard,Left Soft]]
				 }),
     ("encephalon",WordGenerator {
				  softs=["u","ou","iu","uo","ui"],
				  hards=["h","j","m","mm","x","z"],
				  word_patterns=[intersperse (Left Soft) $ replicate 9 (Left Hard)]
				 }),
     ("goliath",WordGenerator {
			       softs=["a","e","i","o","u"],
			       hards=["g","k","d","t","z"],
			       word_patterns=[[Left Hard,Left Hard,Left Soft,Left Hard,Left Hard],
					      [Left Hard,Left Hard,Left Soft,Left Hard]]
			      }),
     ("hellion",WordGenerator {
			       softs=["i","ei","ie","ii","ee"],
			       hards=["ts","tsts","tststs","l","n","non","kak","lor","kol","lok"],
			       word_patterns=[[Left Hard,Left Soft,Left Hard,Right "-",Left Hard,Left Soft,Left Hard],
					      [Left Hard,Left Soft,Right "-",Left Hard,Left Soft,Left Hard],
					      [Left Hard,Left Soft,Left Hard,Right "-",Left Soft,Left Hard]]
			      }),
     ("kraken",WordGenerator {
			      softs=["eee","iii","ouo","uou"],
			      hards=["sl","tch","mnak","klak","chlak","toultk","k","kk","l","ll","ch","mn"],
			      word_patterns=[[Left Soft,Left Hard],
					     [Left Hard,Left Soft],
					     [Left Hard,Left Soft,Left Hard]]
			     }),
     ("myrmidon",WordGenerator {
				softs=["ay","ey","iouy","oy","yr","ya","ye","yi","iy","i","'"],
				hards=["tyr","myr","dyr","nasyr","kyr","jyr","lyr","nyr","don","nod","m"],
				word_patterns=[[Left Hard,Left Hard,Left Soft,Left Hard],
					       [Left Soft,Left Hard,Left Soft,Left Hard,Left Hard]]
				}),
     ("perennial",WordGenerator {
				 softs=["'","-","..","..."],
				 hards=["hush","shh","hhh","fshsh"],
				 word_patterns=[[Left Hard,Left Soft,Left Hard],
						[Left Hard,Left Soft,Left Hard,Left Soft,Left Hard],
						[Left Hard,Left Soft,Left Hard,Left Soft,Left Hard,Left Soft,Left Hard]]
				}),
     ("recreant",WordGenerator {
				softs=["1","2","3","4","5","6","7","8","9"],
				hards=["I","O"],
				word_patterns=[[Left Soft,Left Soft,Left Soft,Right " by ",Left Soft,Left Soft,Left Hard,Left Hard],
					       [Left Soft,Left Soft,Right " of ",Left Hard,Right "1",Left Soft,Left Soft],
					       [Left Hard,Left Soft,Left Hard,Right " ",Left Soft,Right " ",Left Soft,Left Soft,Left Soft,Left Soft,Left Hard]]
			       }),
     ("reptilian",WordGenerator {
				 softs=["ili","ini","ala","aka","apa","ede","eke","ete","unu"],
				 hards=["jaj","jij","jej","juj","pup","pip","pop","pap","lal","lil","nin","non","tat","tet"],
				 word_patterns=[[Left Soft,Left Hard,Left Soft],
						[Left Hard,Left Soft,Left Hard],
						[Left Hard,Left Hard,Left Soft,Left Hard],
						[Left Hard,Left Soft,Left Hard,Left Hard],
						[Left Hard,Left Soft,Left Hard,Left Soft]]
				})
    ]

-- |
-- For english translations, selects between "a" and "an".
a :: String -> String
a str@(x:_) | x `elem` "aeiouAEIOU" = "an " ++ str
a str = "a " ++ str

-- |
-- Performs a translation.
--
translateStr :: Language -> [String] -> String

--
-- English language translation.
--
translateStr English ["window-title"] = "RogueStar - OpenGL"

translateStr English ["menu-title","select-race"] = "Select a species for your starting character:"
translateStr English ["menu-title","select-base-class"] = "Select a class for your starting character:"

translateStr English ["menu-item","select-base-class","reroll"] = "Reroll Character"

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

translateStr English ["a-creature-named",species,random_id] = (titleCapitalize $ a $ translateStr English ["species",species]) ++
							      " named " ++ 
							      (titleCapitalize $ generateName species random_id)

translateStr English ["gender",gender] = titleCapitalize gender

translateStr English ["species",species] = titleCapitalize species

translateStr English ["class","forceadept"] = "Force Adept"
translateStr English ["class",the_class] = titleCapitalize the_class

translateStr English ["table-action","select-race",species] = "----\n" ++ "Welcome, young " ++ species ++ "."

translateStr English ["table-action","select-class",the_class] = 
    ("----" ++ "\n" ++
     "> Two thousand years ago the Ascendant Knights preserved peace in the galaxy through\n" ++
     "> charity and negotiation - and sometimes force.  The knights evolved beyond their\n" ++
     "> physical forms, leaving behind a benevolent but disinterested galactic government\n" ++
     "> called the Interstellar Concordance.  Since then the galaxy has existed in an uneasy\n" ++
     "> peace as tensions mount between the other two galactic powers: the pragmatic Pan\n" ++
     "> Galactic Treaty Organization (PGTO) and the brutal Imperial Alliance.\n" ++
     "> \n" ++
     "> You, a newly trained " ++ class_name ++ ", are vacationing on the luxurious alien\n" ++
     "> planet of Epicurea, when you recieve an alarming signal through your hyperspace\n" ++
     "> reciever . . .\n" ++
     "> \n" ++
     "> \"This is . . . [static] . . . orbiting Myrmidon Prime . . . defense forces caught\n" ++
     "> entirely by surprise . . . destruction on a planetwide scale . . . indicate\n" ++ 
     "> temperatures lower than . . . northern polar region stretching over most of the\n" ++
     "> southeast continent . . . meteorological activity may have ceased entirely and\n" ++
     "> . . . bright flash and radiation sensors spiked just before the . . . certain\n" ++
     "> that the Treaty Organization Headquarters was at the exact epicenter . . . . . .\n" ++
     "> . . . . . . [static] . . . . . .\"\n" ++
     "> \n" ++
     "> The galaxy is at war.")
	where class_name = (translateStr English ["class",the_class])

-- 
-- General redirection of translateStr requests that probably apply to all languages.
--
translateStr language ["menu-item","select-race",species] = translateStr language ["species",species]
translateStr language ["menu-item","select-base-class",the_class] = translateStr language ["class",the_class]
translateStr language ["table-data",_,"gender",gender] = translateStr language ["gender",gender]
translateStr language ["table-data",_,stat_name,stat_value] | stat_name `elem` ["str","dex","con","int","per","cha","mind"]
								= translateStr language ["statistic",stat_value]
translateStr language ["statistic",stat_value] = (padRight 15 $ translateStr language ["statistic-qualitative",statQuantityToQuality stat_value]) ++ padLeft 6 ("(" ++ stat_value ++ ")")

--
-- If all other cases fail, print the untranslated data in all upper case.
--
translateStr _ args = strToUpper $ unwords args
