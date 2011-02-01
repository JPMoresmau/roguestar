module SpeciesData
    (Species(..),
     all_species,
     player_species)
    where

data Species =
     Anachronid
   | Androsynth
   | Ascendant
   | Caduceator
   | DustVortex
   | Encephalon
   | Goliath
   | Hellion
   | Kraken
   | Myrmidon
   | Perennial
   | Recreant
   | Reptilian
       deriving (Eq,Ord,Bounded,Enum,Read,Show)

all_species :: [Species]
all_species = [minBound..maxBound]

player_species :: [Species]
player_species = [
    Anachronid,
    Androsynth,
    Ascendant,
    Caduceator,
    Encephalon,
    Goliath,
    Hellion,
    Kraken,
    Myrmidon,
    Perennial,
    Recreant,
    Reptilian]

