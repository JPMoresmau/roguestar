
module Substances
    (Gas(..),
     Material(..),
     Chromalite(..),
     Solid(..),
     materialValue,
     MaterialValue(..),
     Substance,
     substances,
     prettySubstance,
     printSubstances,
     gasWeight,
     chromaliteAlignment,
     chromalitePotency)
    where

import Alignment
import Data.List
import Data.Ord

data Substance = 
    GasSubstance Gas
  | MaterialSubstance Material
  | ChromaliteSubstance Chromalite
             deriving (Read,Show,Eq,Ord)

substances :: [Substance]
substances = map GasSubstance [minBound..maxBound] ++
             map MaterialSubstance [minBound..maxBound] ++
	     map ChromaliteSubstance [minBound..maxBound]

prettySubstance :: Substance -> String
prettySubstance (GasSubstance x) = show x
prettySubstance (MaterialSubstance x) = show x
prettySubstance (ChromaliteSubstance x) = show x

printSubstances :: IO ()
printSubstances = putStrLn $ unlines $ map (\(x,y) -> prettySubstance y ++ ":  " ++ show x) $ sortBy (comparing fst) $ map (\x -> (substanceValue x,x)) substances

data Solid = MaterialSolid Material
           | ChromaliteSolid Chromalite
           deriving (Read,Show,Eq,Ord)
             
data Gas = 
    Hydrogen
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
	
data Material = 
    Aluminum
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
  | Uranium
  | Plutonium
  | Thorium
  | Diamond
  | Carbon
  | Wood
  | Plastic
        deriving (Eq,Enum,Ord,Show,Read,Bounded)

--
-- Chromalite is an engineered, crystaline metamaterial capable of storing many times it's own rest mass energy.
-- Precisely how many times is indicated by the chromalitePotency function.
--
-- Because any accidental release of this energy would obviously be catastrophic, chromalite is itself intelligent
-- and capable of adapting to stressful situations to avoid any such accidental release.
--
data Chromalite = 
    Rutilium     -- red Chromalite
  | Crudnium     -- green Chromalite
  | Pteulanium   -- blue Chromalite
  | Caerulite    -- azure Chromalite
  | Ionidium     -- violet Chromalite
  | Aurite       -- yellow Chromalite
  | Argentate    -- silver Chromalite
  | Trabanate    -- brown Chromalite
  | Arumate      -- gold Chromalite
  | Candonium    -- white Chromalite
  | Canitium     -- gray Chromalite
  | Infuscanoid  -- black Chromalite
  | Endurium     -- blue/shadowy Chromalite
  | Malignite    -- yellow/shadowy Chromalite
  | Diabolite    -- radiant white Chromalite
  | Bectonite    -- radiant black Chromalite
     deriving (Eq,Enum,Ord,Show,Read,Bounded)

data MaterialValue = MaterialValue {
    material_construction_value :: Integer, -- value of material for constructing buildings, pipes, casings for gadgets, etc
    material_critical_value :: Integer,     -- value of material for critical purposes, such as miniature electronic components
    material_scarcity :: Integer }          -- how rare the material is in nature and by synthesis

gasWeight :: Gas -> Integer
gasWeight Hydrogen = 1
gasWeight Helium = 4
gasWeight Oxygen = 16
gasWeight Nitrogen = 14
gasWeight Flourine = 19
gasWeight Neon = 20
gasWeight Argon = 40
gasWeight Krypton = 84
gasWeight Xenon = 131
gasWeight Radon = 222
gasWeight Chlorine = 35

materialValue :: Material -> MaterialValue
materialValue Aluminum =    MaterialValue   10  10  10
materialValue Titanium =    MaterialValue   15  10  20
materialValue Palladium =   MaterialValue    2 150   5
materialValue Molybdenum =  MaterialValue    1  50   3
materialValue Lead =        MaterialValue    3  20   2
materialValue Copper =      MaterialValue    8  80  15
materialValue Iron =        MaterialValue    5  10   2
materialValue Cobalt =      MaterialValue    3  60   7
materialValue Zirconium =   MaterialValue    2  40  10
materialValue Gold =        MaterialValue    4  20  50
materialValue Silver =      MaterialValue    3  30  20
materialValue Platinum =    MaterialValue    1 100  70
materialValue Zinc =        MaterialValue    6  50   4
materialValue Uranium =     MaterialValue    1 300  40
materialValue Plutonium =   MaterialValue    1 500 100
materialValue Thorium =     MaterialValue    2 200   4
materialValue Diamond =     MaterialValue   40  20  15
materialValue Carbon =      MaterialValue    2  20   1
materialValue Wood =        MaterialValue    3   0   2
materialValue Plastic =     MaterialValue    4   0   2

chromaliteAlignment :: Chromalite -> Alignment
chromaliteAlignment Rutilium = (Chaotic,Strategic)
chromaliteAlignment Crudnium = (Neutral,Strategic)
chromaliteAlignment Pteulanium = (Lawful,Strategic)
chromaliteAlignment Caerulite = (Lawful,Tactical)
chromaliteAlignment Ionidium = (Neutral,Tactical)
chromaliteAlignment Aurite = (Chaotic,Tactical)
chromaliteAlignment Argentate = (Lawful,Diplomatic)
chromaliteAlignment Trabanate = (Neutral,Diplomatic)
chromaliteAlignment Arumate = (Chaotic,Diplomatic)
chromaliteAlignment Candonium = (Lawful,Indifferent)
chromaliteAlignment Canitium = (Neutral,Indifferent)
chromaliteAlignment Infuscanoid = (Chaotic,Indifferent)
chromaliteAlignment Endurium = (Evil,Strategic)
chromaliteAlignment Malignite = (Evil,Tactical)
chromaliteAlignment Diabolite = (Evil,Diplomatic)
chromaliteAlignment Bectonite = (Evil,Indifferent)

class SubstanceType a where
    substanceValue :: a -> Integer
    toSubstance :: a -> Substance

instance SubstanceType Gas where
    substanceValue x = gasWeight x ^ 2 - gasWeight x
    toSubstance x = GasSubstance x
    
instance SubstanceType Material where
    substanceValue x = nom * crit * scarce + nom + crit + scarce
        where MaterialValue nom crit scarce = materialValue x
    toSubstance x = MaterialSubstance x

instance SubstanceType Chromalite where
    substanceValue x = 10 * chromalitePotency x ^ 2 + 100 * chromalitePotency x
    toSubstance x = ChromaliteSubstance x

instance SubstanceType Substance where
    substanceValue (GasSubstance x) = substanceValue x
    substanceValue (MaterialSubstance x) = substanceValue x
    substanceValue (ChromaliteSubstance x) = substanceValue x
    toSubstance x = x

instance SubstanceType Solid where
    substanceValue = substanceValue . toSubstance
    toSubstance (MaterialSolid x) = toSubstance x
    toSubstance (ChromaliteSolid x) = toSubstance x

chromalitePotency :: Chromalite -> Integer
chromalitePotency = alignmentPotency . chromaliteAlignment
