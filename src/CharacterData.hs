
module CharacterData
    (CharacterClass(..),
     all_character_classes,
     base_character_classes)
    where

data CharacterClass = Barbarian
		    | Consular
		    | Engineer
		    | ForceAdept
		    | Marine
		    | Ninja
		    | Pilot
		    | Privateer
		    | Scout
		    | Shepherd
		    | Thief
		    | Warrior
		    deriving (Eq,Enum,Bounded,Read,Show)

all_character_classes :: [CharacterClass]
all_character_classes = [minBound..maxBound]

base_character_classes :: [CharacterClass]
base_character_classes = [Barbarian,
			  Consular,
			  Engineer,
			  ForceAdept,
			  Marine,
			  Ninja,
			  Pilot,
			  Privateer,
			  Scout,
			  Shepherd,
			  Thief,
			  Warrior]
