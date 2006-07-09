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

module PeriodicTable
    (Element(..),displayPeriodicTable,elementAlignment)
    where

import Alignment

-- For Metals, the first integer is the normal material value, the second is an exceptional material value.
-- When making repairs, the exceptional material value is used 1/20th of the time, the normal otherwise.
-- For Fuels, the integer represents the fuel value of the element.
-- Metals', and Metaloids' alignments indicate how artifacts behave when made out of those materials.
data ElementData = Metal Integer Integer (Maybe Alignment)
		 | Fuel Integer
		 | Gas
		 | Chromalite Alignment

data Element = HydrogenGas -- Gasses
	     | HeliumGas
	     | OxygenGas
	     | NitrogenGas
	     | FlourineGas
	     | NeonGas
	     | ArgonGas
	     | KryptonGas
	     | XenonGas
	     | RadonGas
	     | ChlorineGas
	     | DeuteroniumFuel  -- Fuels
	     | TritoniumFuel
	     | UraniumFuel
	     | PlutoniumFuel
	     | ThoriumFuel
	     | EnduriumFuel
	     | AluminumMetal-- Metals
	     | TitaniumMetal
	     | PalladiumMetal
	     | MolybdenumMetal
	     | LeadMetal
	     | CopperMetal
	     | IronMetal
	     | CobaltMetal
	     | ZirconiumMetal
	     | GoldMetal
	     | SilverMetal
	     | PlatinumMetal
	     | ZincMetal
	     | MetallicCarbon
	     | Rutilium -- red Chromalite
	     | Crudnium -- green Chromalite
	     | Puteulanium -- blue Chromalite
	     | Candoinum -- white Chromalite
	     | Canitium -- gray Chromalite
	     | Infuscanoid -- black Chromalite
	     | Argentate -- silver Chromalite
	     | Trabanate -- brown Chromalite
	     | Arumate -- gold Chromalite
	     | Svartium -- bright Chromalite
	     | Adomite -- red Metachromalite
	     | Yarokate -- green Metachromalite
	     | Kakohlate -- blue Metachromalite
	     | Lahvanium -- white Metachromalite
	     | Afrate -- gray Metachromalite
	     | Shahkrate -- black Metachromalite
	     | Tekletium -- silver Metachromalite
	     | Koomite -- brown Metachromalite
	     | Zahovate -- gold Metachromalite
	     | Vitrium -- dark Chromalite
	       deriving (Eq,Enum,Ord,Show,Read)

--
-- Answers the type of an element.
--
elementData :: Element -> ElementData
elementData HydrogenGas = Gas
elementData HeliumGas = Gas
elementData NitrogenGas = Gas
elementData OxygenGas = Gas
elementData FlourineGas = Gas
elementData NeonGas = Gas
elementData ArgonGas = Gas
elementData KryptonGas = Gas
elementData XenonGas = Gas
elementData RadonGas = Gas
elementData ChlorineGas = Gas
elementData DeuteroniumFuel = Fuel 2
elementData TritoniumFuel = Fuel 3
elementData UraniumFuel = Fuel 10
elementData PlutoniumFuel = Fuel 12
elementData EnduriumFuel = Fuel 1000
elementData ThoriumFuel = Fuel 15
elementData AluminumMetal = Metal 8 2 Nothing
elementData TitaniumMetal = Metal 13 3 Nothing
elementData PalladiumMetal = Metal 1 144 Nothing
elementData MolybdenumMetal = Metal 1 8 Nothing
elementData LeadMetal = Metal 1 1 Nothing
elementData CopperMetal = Metal 3 5 Nothing
elementData IronMetal = Metal 3 1 Nothing
elementData ZirconiumMetal = Metal 1 13 Nothing
elementData CobaltMetal = Metal 5 55 Nothing
elementData GoldMetal = Metal 2 89 (Just (Weak,Chaotic,Diplomatic))
elementData SilverMetal = Metal 2 21 (Just (Weak,Lawful,Diplomatic))
elementData PlatinumMetal = Metal 2 34 Nothing
elementData ZincMetal = Metal 5 15 Nothing
elementData MetallicCarbon = Metal 25 25 (Just (Weak,Evil,Tactical))
elementData Rutilium = Chromalite (Moderate,Chaotic,Tactical) -- red Chromalite
elementData Crudnium = Chromalite (Moderate,Neutral,Tactical) -- green Chromalite
elementData Puteulanium = Chromalite (Moderate,Lawful,Tactical) -- blue Chromalite
elementData Candoinum = Chromalite (Moderate,Lawful,Strategic) -- white Chromalite
elementData Canitium = Chromalite (Moderate,Neutral,Strategic) -- gray Chromalite
elementData Infuscanoid = Chromalite (Moderate,Chaotic,Strategic) -- black Chromalite
elementData Argentate = Chromalite (Moderate,Lawful,Diplomatic) -- silver Chromalite
elementData Trabanate = Chromalite (Moderate,Neutral,Diplomatic) -- brown Chromalite
elementData Arumate = Chromalite (Moderate,Neutral,Diplomatic) -- gold Chromalite
elementData Svartium = Chromalite (Moderate,Evil,Strategic) -- dark Chromalite
elementData Adomite = Chromalite (Strong,Chaotic,Tactical) -- red Metachromalite
elementData Yarokate = Chromalite (Strong,Neutral,Tactical) -- green Metachromalite
elementData Kakohlate = Chromalite (Strong,Lawful,Tactical) -- blue Metachromalite
elementData Lahvanium = Chromalite (Strong,Lawful,Strategic) -- white Metachromalite
elementData Afrate = Chromalite (Strong,Neutral,Strategic) -- gray Metachromalite
elementData Shahkrate = Chromalite (Strong,Chaotic,Strategic) -- black Metachromalite
elementData Tekletium = Chromalite (Strong,Lawful,Diplomatic) -- silver Metachromalite
elementData Koomite = Chromalite (Strong,Neutral,Diplomatic) -- brown Metachromalite
elementData Zahovate = Chromalite (Strong,Chaotic,Diplomatic) -- gold Metachromalite
elementData Vitrium = Chromalite (Strong,Evil,Diplomatic) -- bright Chromalite

--
-- Answers the practical economic value of the element (an estimate of the element's usefulness
-- to be used when deciding the value of the element in trade).
--
elementValue :: Element -> Integer
elementValue element = case (elementData element) of
					          Gas -> 1
					          Fuel fuel_val -> fuel_val
					          Metal val crit_val align -> (10*val + crit_val) + (elementAlignmentValue align)
					          Chromalite align -> elementAlignmentValue (Just align)

elementStrengthValue :: AlignmentStrength -> Integer
elementStrengthValue Weak = 1
elementStrengthValue Moderate = 5
elementStrengthValue Strong = 15

elementEthicalValue :: AlignmentEthic -> Integer
elementEthicalValue Lawful = 5
elementEthicalValue Chaotic = 3
elementEthicalValue Neutral = 1
elementEthicalValue Evil = 12

elementSchoolValue :: AlignmentSchool -> Integer
elementSchoolValue Strategic = 3
elementSchoolValue Tactical = 2
elementSchoolValue Diplomatic = 5
elementSchoolValue Indifferent = 1

elementAlignmentValue :: Maybe Alignment -> Integer
elementAlignmentValue (Just (strength,ethical,schl)) = (elementStrengthValue strength) *
						       (elementEthicalValue ethical) *
						       (elementSchoolValue schl)
elementAlignmentValue Nothing = elementStrengthValue Weak * elementEthicalValue Neutral * elementSchoolValue Indifferent

--
-- Returns the fuel points for this element.  Most elements have
-- zero fuel points, indicating that they are not fuel.
--
elementFuelPoints :: Element -> Integer
elementFuelPoints element = case (elementData element) of
						       Gas -> 0
						       Fuel fuel_value -> fuel_value
						       Metal _ _ _ -> 0
						       Chromalite _ -> 0

--
-- Returns (the normal material points, the critical material poitns)
-- for the given element.  Many elements have material points of
-- (0,0) indicating that they have no use as construction materials.
--
elementMaterialPoints2 :: Element -> (Integer,Integer)
elementMaterialPoints2 element = case (elementData element) of
						            Gas -> (0,0)
						            Fuel _ -> (0,0)
						            Metal norm crit _ -> (norm,crit)
						            Chromalite _ -> (0,0)

--
-- Returns the material points for the specified element for normal
-- material needs.
--
elementNormalMaterialPoints :: Element -> Integer
elementNormalMaterialPoints element = fst (elementMaterialPoints2 element)

--
-- Returns the material points for the specified element for critical
-- material needs.  Distinction: a spaceship might be made of 99%
-- titanium, but need a small amount of zirconium for a critical
-- component.  Normal needs represents 99% of needs, while critical
-- needs represents those special components.
--
elementCriticalMaterialPoints :: Element -> Integer
elementCriticalMaterialPoints element = snd (elementMaterialPoints2 element)

--
-- Returns the alignment of this element.  All elements have alignment,
-- but usually it is (Weak,Neutral,Indifferent), which is the closest
-- thing in the game to a completely neutral alignment.
--
elementAlignment :: Element -> Alignment
elementAlignment element = case (elementData element) of
                                                      Gas -> (Weak,Neutral,Indifferent)
						      Fuel _ -> (Weak,Neutral,Indifferent)
						      Metal _ _ (Just align) -> align
                                                      Metal _ _ Nothing -> (Weak,Neutral,Indifferent)
						      Chromalite align -> align

--
-- Displays the roguestar periodic table of elements.
--
displayPeriodicTable :: IO ()

displayPeriodicTable = do putStrLn "displaying-table periodic-table"
			  putStrLn "table-format name value fuel-points normal-material-points critical-material-points"
			  displayPeriodicTable_ (enumFrom (toEnum 0))
			  putStrLn "done-table"

displayPeriodicTable_ :: [Element] -> IO ()
displayPeriodicTable_ elems = mapM_ displayElement elems

displayElement :: Element -> IO ()

displayElement element = do putStrLn ((show element) ++ " " ++ (show (elementValue element)) ++ " " ++ (show (elementFuelPoints element)) ++ " " ++
				      (show (elementNormalMaterialPoints element)) ++ " " ++ (show (elementCriticalMaterialPoints element)))