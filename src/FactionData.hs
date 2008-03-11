
module FactionData
    (Faction(..))
    where

data Faction = Player
	     | InterstellarConcordance        -- the lawful galactic government
	     | PanGalacticTreatyOrganization  -- the neutral galactic government
	     | ImperialAlliance               -- the chaotic galactic government
	     | MonstersInc                    -- nonsentient monsters
	     | Nonaligned                     -- pirates, mecenaries, your friendly neighborhood police office, etc 
	     | Cyborgs                        -- cyborgs
	     | Whispers                       -- the dark indifferent destroyers of worlds
	     | Proselytes                     -- evil entities that possess others' minds
	     | Civilian                       -- merchants, children -- don't kill these
	       deriving (Eq,Read,Show)
