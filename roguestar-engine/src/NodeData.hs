module NodeData
    (nodeEffect)
    where

import BuildingData
import CreatureData
import CharacterData
import CharacterAdvancement

nodeEffect :: NodeType -> CharacterBumpRequest
nodeEffect Anchor = AwardCharacter 1
nodeEffect Monolith = ForceCharacter StarChild

