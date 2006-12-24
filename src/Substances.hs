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

module Substances
    (Gas(..),
     Fuel(..),
     Metal(..),
     Chromalite(..),
     Solid(..),
     Element,
     elements,
     isGas,
     isFuel,
     isMetal,
     isChromalite,
     toGas,
     toFuel,
     toMetal,
     toChromalite,
     isSolid,
     toSolid,
     chromaliteAlignment,
     chromalitePotency)
    where

import Alignment

data Element = GasElement Gas
             | FuelElement Fuel
             | MetalElement Metal
             | ChromaliteElement Chromalite
             deriving (Read,Show,Eq,Ord)
             
data Solid = MetalSolid Metal
           | ChromaliteSolid Chromalite
           deriving (Read,Show,Eq,Ord)
             
elements :: [Element]
elements = map toElement [minBound..maxBound :: Gas] ++
           map toElement [minBound..maxBound :: Fuel] ++
           map toElement [minBound..maxBound :: Metal] ++
           map toElement [minBound..maxBound :: Chromalite]
             
isGas :: Element -> Bool
isGas (GasElement _) = True
isGas _ = False

isFuel :: Element -> Bool
isFuel (FuelElement _) = True
isFuel _ = False

isMetal :: Element -> Bool
isMetal (MetalElement _) = True
isMetal _ = False

isChromalite :: Element -> Bool
isChromalite (ChromaliteElement _) = True
isChromalite _ = False

isSolid :: Element -> Bool
isSolid x = isMetal x || isChromalite x

toSolid :: (Elemental a) => a -> Solid
toSolid elemental = case toElement elemental of
                                             MetalElement x -> MetalSolid x
                                             ChromaliteElement x -> ChromaliteSolid x
                                             _ -> error "toSolid: not a solid"

toGas :: Element -> Gas
toGas (GasElement x) = x
toGas _ = error "toGas: not a Gas"

toFuel :: Element -> Fuel
toFuel (FuelElement x) = x
toFuel _ = error "toFuel: not a Fuel"

toMetal :: Element -> Metal
toMetal (MetalElement x) = x
toMetal _ = error "toMetal: not a Metal"

toChromalite :: Element -> Chromalite
toChromalite (ChromaliteElement x) = x
toChromalite _ = error "toChromalite: not a Chromalite"

data Gas = Hydrogen -- Gasses
	     | Helium
	     | Oxygen
	     | Nitrogen
	     | Flourine
	     | Neon
	     | Argon
	     | Krypton
	     | Xenon
	     | Radon
	     | Chlorine deriving (Eq,Enum,Ord,Show,Read,Bounded)
	
data Fuel = Deuteronium  -- Fuels
	  | Tritonium
	  | Uranium
	  | Plutonium
	  | Thorium
	  | Endurium deriving (Eq,Enum,Ord,Show,Read,Bounded)
	  
data Metal = Aluminum-- Metals
	   | Titanium
	   | Palladium
           | Molybdenum
           | Lead
           | Copper
           | Iron
           | Cobalt
           | Zirconium
           | Gold
           | Silver
           | Platinum
           | Zinc
           | MetallicCarbon deriving (Eq,Enum,Ord,Show,Read,Bounded)
	     
data Chromalite = Rutilium -- red Chromalite
	        | Crudnium -- green Chromalite
	        | Pteulanium -- blue Chromalite
	        | Candonium -- white Chromalite
	        | Canitium -- gray Chromalite
	        | Infuscanoid -- black Chromalite
	        | Argentate -- silver Chromalite
	        | Trabanate -- brown Chromalite
	        | Arumate -- gold Chromalite
	        | Svartium -- dark Chromalite
	        | Adomite -- red Metachromalite
	        | Yarokate -- green Metachromalite
	        | Kakohlate -- blue Metachromalite
	        | Lahvanium -- white Metachromalite
	        | Afrate -- gray Metachromalite
	        | Shahkrate -- black Metachromalite
	        | Tekletium -- silver Metachromalite
	        | Koomite -- brown Metachromalite
	        | Zahovate -- gold Metachromalite
	        | Vitrium -- bright Chromalite
	        | Bectonite -- corrupt Chromalite
	          deriving (Eq,Enum,Ord,Show,Read,Bounded)

fuelValue :: (Elemental a) => a -> Integer
fuelValue e = case toElement e of
                               FuelElement x -> fuelValue_ x
                               _ -> 0

fuelValue_ :: Fuel -> Integer
fuelValue_ Deuteronium = 2
fuelValue_ Tritonium = 3
fuelValue_ Uranium = 10
fuelValue_ Plutonium = 12
fuelValue_ Endurium = 1000
fuelValue_ Thorium = 15

materialValue :: (Elemental a) => a -> (Integer,Integer)
materialValue e = case toElement e of
                                   MetalElement x -> materialValue_ x
                                   _ -> (0,0)

materialValue_ :: Metal -> (Integer,Integer)
materialValue_ Aluminum = (8,2)
materialValue_ Titanium = (13,3)
materialValue_ Palladium = (1,144)
materialValue_ Molybdenum = (1,8)
materialValue_ Lead = (1,1)
materialValue_ Copper = (3,5)
materialValue_ Iron = (3,1)
materialValue_ Zirconium = (1,13)
materialValue_ Cobalt = (5,55)
materialValue_ Gold = (2,89)
materialValue_ Silver = (2,21)
materialValue_ Platinum = (2,34)
materialValue_ Zinc = (5,15)
materialValue_ MetallicCarbon = (25,25)

chromaliteAlignment :: (Elemental a) => a -> Maybe Alignment
chromaliteAlignment e = case toElement e of
                                         ChromaliteElement x -> Just $ chromaliteAlignment_ x
                                         _ -> Nothing

chromaliteAlignment_ :: Chromalite -> Alignment
chromaliteAlignment_ Rutilium = (Moderate,Chaotic,Tactical) -- red Chromalite
chromaliteAlignment_ Crudnium = (Moderate,Neutral,Tactical) -- green Chromalite
chromaliteAlignment_ Pteulanium = (Moderate,Lawful,Tactical) -- blue Chromalite
chromaliteAlignment_ Candonium = (Moderate,Lawful,Strategic) -- white Chromalite
chromaliteAlignment_ Canitium = (Moderate,Neutral,Strategic) -- gray Chromalite
chromaliteAlignment_ Infuscanoid = (Moderate,Chaotic,Strategic) -- black Chromalite
chromaliteAlignment_ Argentate = (Moderate,Lawful,Diplomatic) -- silver Chromalite
chromaliteAlignment_ Trabanate = (Moderate,Neutral,Diplomatic) -- brown Chromalite
chromaliteAlignment_ Arumate = (Moderate,Neutral,Diplomatic) -- gold Chromalite
chromaliteAlignment_ Svartium = (Moderate,Evil,Tactical) -- dark Chromalite
chromaliteAlignment_ Adomite = (Strong,Chaotic,Tactical) -- red Metachromalite
chromaliteAlignment_ Yarokate = (Strong,Neutral,Tactical) -- green Metachromalite
chromaliteAlignment_ Kakohlate = (Strong,Lawful,Tactical) -- blue Metachromalite
chromaliteAlignment_ Lahvanium = (Strong,Lawful,Strategic) -- white Metachromalite
chromaliteAlignment_ Afrate = (Strong,Neutral,Strategic) -- gray Metachromalite
chromaliteAlignment_ Shahkrate = (Strong,Chaotic,Strategic) -- black Metachromalite
chromaliteAlignment_ Tekletium = (Strong,Lawful,Diplomatic) -- silver Metachromalite
chromaliteAlignment_ Koomite = (Strong,Neutral,Diplomatic) -- brown Metachromalite
chromaliteAlignment_ Zahovate = (Strong,Chaotic,Diplomatic) -- gold Metachromalite
chromaliteAlignment_ Vitrium = (Strong,Evil,Diplomatic) -- bright Chromalite
chromaliteAlignment_ Bectonite = (Moderate,Evil,Strategic)

class Elemental a where
    elementValue :: a -> Integer
    toElement :: a -> Element

instance Elemental Gas where
    elementValue _ = 1
    toElement x = GasElement x
    
instance Elemental Fuel where
    elementValue x = fuelValue x ^ 2
    toElement x = FuelElement x

instance Elemental Metal where
    elementValue x = let (norm,crit) = materialValue x
                         in 10*norm + crit
    toElement x = MetalElement x

instance Elemental Chromalite where
    elementValue = elementAlignmentValue . chromaliteAlignment_
    toElement x = ChromaliteElement x

instance Elemental Element where
    elementValue (GasElement x) = elementValue x
    elementValue (FuelElement x) = elementValue x
    elementValue (MetalElement x) = elementValue x
    elementValue (ChromaliteElement x) = elementValue x
    toElement x = x

instance Elemental Solid where
    elementValue = elementValue . toElement
    toElement (MetalSolid x) = toElement x
    toElement (ChromaliteSolid x) = toElement x

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

elementAlignmentValue :: Alignment -> Integer
elementAlignmentValue (strength,ethical,schl) = (elementStrengthValue strength) *
						(elementEthicalValue ethical) *
						(elementSchoolValue schl)
						
chromalitePotency :: Chromalite -> Integer
chromalitePotency c = case chromaliteAlignment_ c of
                                                  (strength,ethical,schl) -> elementStrengthValue strength + elementEthicalValue ethical + elementSchoolValue schl