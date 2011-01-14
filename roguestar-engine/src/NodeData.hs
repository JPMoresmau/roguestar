module NodeData
    ()
    where

import BuildingData
import CreatureData
import CharacterData

data NodeEffect =
    ClassBonus CharacterClass
  | PointBonus Integer

nodeEffect :: NodeType -> NodeEffect
nodeEffect Anchor = PointBonus 1
nodeEffect Monolith = ClassBonus StarChild

instance CreatureEndo NodeEffect where
    applyToCreature (PointBonus bonus) c = bumpCharacter bonus c
    applyToCreature (ClassBonus bonus) c = applyToCreature bonus c

instance CreatureEndo NodeType where
    applyToCreature n c = applyToCreature (nodeEffect n) c

