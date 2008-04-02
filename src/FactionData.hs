
module FactionData
    (Faction(..))
    where

data Faction = Player
	     | InterstellarConcordance        -- the lawful galactic government
	     | PanGalacticTreatyOrganization  -- the neutral galactic government
	     | ImperialAlliance               -- the chaotic galactic government
	     | Monsters                       -- nonsentient monsters (indifferent "government")
	     | Pirates                        -- pirates (tactical "government")
	     | Cyborgs                        -- cyborgs (strategic "government")
	     | SocialUtopiate                 -- an economic quasi-alliance or super-clan (diplomatic "government")
	     | Whispers                       -- the dark indifferent destroyers of worlds
	     | Proselytes                     -- evil entities that possess others' minds
	     | Civilian                       -- merchants, children -- don't kill these
	       deriving (Eq,Read,Show)
