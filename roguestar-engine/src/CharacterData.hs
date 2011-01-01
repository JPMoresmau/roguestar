
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
                    | Pirate
                    | Scout
                    | Shepherd
                    | Thief
                    | Warrior
                    deriving (Eq,Enum,Bounded,Read,Show,Ord)

all_character_classes :: [CharacterClass]
all_character_classes = [minBound..maxBound]

base_character_classes :: [CharacterClass]
base_character_classes = [Barbarian,
                          Consular,
                          Engineer,
                          ForceAdept,
                          Marine,
                          Ninja,
                          Pirate,
                          Scout,
                          Shepherd,
                          Thief,
                          Warrior]

