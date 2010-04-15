{-# LANGUAGE OverloadedStrings #-}
module Substances
    (Gas(..),
     Material(..),
     Chromalite(..),
     Solid(..),
     materialValue,
     MaterialValue(..),
     Substance(..),
     SubstanceType(toSubstance),
     coerceSubstance,
     isGas,
     isMaterial,
     isChromalite,
     substances,
     prettySubstance,
     printSubstances,
     gasValue,
     chromaliteAlignment,
     chromalitePotency)
    where

import Alignment
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.ByteString.Char8 as B

data Substance = 
    GasSubstance Gas
  | MaterialSubstance Material
  | ChromaliteSubstance Chromalite
             deriving (Read,Show,Eq,Ord)

substances :: [Substance]
substances = map GasSubstance [minBound..maxBound] ++
             map MaterialSubstance [minBound..maxBound] ++
	     map ChromaliteSubstance [minBound..maxBound]

prettySubstance :: Substance -> B.ByteString
prettySubstance (GasSubstance x) = B.pack $ show x
prettySubstance (MaterialSubstance x) = B.pack $ show x
prettySubstance (ChromaliteSubstance x) = B.pack $ show x

printSubstances :: IO ()
printSubstances = B.putStrLn $ B.unlines $ map (\(x,y) -> prettySubstance y `B.append` ":  " `B.append` B.pack (show x)) $ sortBy (comparing fst) $ map (\x -> (substanceValue x,x)) substances

data Solid = MaterialSolid Material
           | ChromaliteSolid Chromalite
           deriving (Read,Show,Eq,Ord)
             
data Gas = 
    Water
  | Hydrogen
  | Helium
  | Oxygen
  | Nitrogen
  | Flourine
  | Neon
  | Argon
  | Krypton
  | Xenon
  | Radon
  | Methane
  | Ammonia
  | Iodine
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
  | Silicon
  | Nickel
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

gasValue :: Gas -> Integer
gasValue Water = 2
gasValue Hydrogen = 4
gasValue Helium = 6
gasValue Nitrogen = 7
gasValue Oxygen = 10
gasValue Flourine = 12
gasValue Neon = 20
gasValue Ammonia = 21
gasValue Methane = 24
gasValue Chlorine = 30
gasValue Argon = 40
gasValue Krypton = 42
gasValue Xenon = 60
gasValue Radon = 70
gasValue Iodine = 100

data MaterialValue = MaterialValue {
    material_construction_value :: Integer, -- value of material for constructing buildings, pipes, casings for gadgets, etc
    material_critical_value :: Integer,     -- value of material for critical purposes, such as miniature electronic components
    material_scarcity :: Integer }          -- scarcity of material

materialValue :: Material -> MaterialValue
materialValue Aluminum =    MaterialValue  50  20   6
materialValue Titanium =    MaterialValue  70  15  15
materialValue Palladium =   MaterialValue  30  30  65
materialValue Molybdenum =  MaterialValue  18  55  40
materialValue Lead =        MaterialValue  15   7  31
materialValue Copper =      MaterialValue  40  40  18
materialValue Iron =        MaterialValue  25  15  10
materialValue Cobalt =      MaterialValue  30  35  30
materialValue Zirconium =   MaterialValue  12  50  23
materialValue Gold =        MaterialValue  20  35  83
materialValue Silver =      MaterialValue  10  20  80
materialValue Platinum =    MaterialValue  22  40  81
materialValue Zinc =        MaterialValue  35  25  26
materialValue Uranium =     MaterialValue   5  90  37
materialValue Plutonium =   MaterialValue   1 100 100
materialValue Thorium =     MaterialValue  20  80  33
materialValue Diamond =     MaterialValue 100 100  90
materialValue Carbon =      MaterialValue  60  20  20
materialValue Wood =        MaterialValue  25   1   3
materialValue Plastic =     MaterialValue  30  10   1
materialValue Silicon =     MaterialValue  25  50   5
materialValue Nickel =      MaterialValue  25  45  25

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
    toSubstance :: a -> Substance
    fromSubstance :: Substance -> Maybe a

coerceSubstance :: (SubstanceType a,SubstanceType b) => a -> Maybe b
coerceSubstance = fromSubstance . toSubstance

isGas :: (SubstanceType a) => a -> Bool
isGas = isJust . (`asTypeOf` (undefined :: Maybe Gas)) . coerceSubstance

isMaterial :: (SubstanceType a) => a -> Bool
isMaterial = isJust . (`asTypeOf` (undefined :: Maybe Material)) . coerceSubstance

isChromalite :: (SubstanceType a) => a -> Bool
isChromalite = isJust . (`asTypeOf` (undefined :: Maybe Chromalite)) . coerceSubstance

substanceValue :: (SubstanceType a) => a -> Integer
substanceValue a = case toSubstance a of
    GasSubstance x -> gasValue x + 10
    MaterialSubstance x -> (nom + crit) * scarce
        where MaterialValue nom crit scarce = materialValue x
    ChromaliteSubstance x -> 1000 + 2 * chromalitePotency x ^ 2

instance SubstanceType Gas where
    toSubstance x = GasSubstance x
    fromSubstance (GasSubstance x) = Just x
    fromSubstance _ = Nothing
    
instance SubstanceType Material where
    toSubstance x = MaterialSubstance x
    fromSubstance (MaterialSubstance x) = Just x
    fromSubstance _ = Nothing

instance SubstanceType Chromalite where
    toSubstance x = ChromaliteSubstance x
    fromSubstance (ChromaliteSubstance x) = Just x
    fromSubstance _ = Nothing

instance SubstanceType Substance where
    toSubstance x = x
    fromSubstance = Just

instance SubstanceType Solid where
    toSubstance (MaterialSolid x) = toSubstance x
    toSubstance (ChromaliteSolid x) = toSubstance x
    fromSubstance (MaterialSubstance x) = Just $ MaterialSolid x
    fromSubstance (ChromaliteSubstance x) = Just $ ChromaliteSolid x
    fromSubstance _ = Nothing

chromalitePotency :: Chromalite -> Integer
chromalitePotency = alignmentPotency . chromaliteAlignment
