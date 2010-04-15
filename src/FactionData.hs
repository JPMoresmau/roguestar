{-# LANGUAGE OverloadedStrings #-}
module FactionData
    (Faction(..),factionPrefix)
    where

import qualified Data.ByteString.Char8 as B

data Faction = Player
	     | InterstellarConcordance        -- the lawful galactic government
	     | PanGalacticTreatyOrganization  -- the neutral galactic government
	     | ImperialAlliance               -- the chaotic galactic government
	     | Monsters                       -- nonsentient monsters (indifferent "government")
	     | Pirates                        -- pirates (tactical "government")
	     | Cyborgs                        -- cyborgs (strategic "government")
	     | SocialUtopiate                 -- an economic super-alliance (diplomatic "government")
	     | Whispers                       -- the dark indifferent destroyers of worlds
	     | Proselytes                     -- evil entities that possess others' minds
	     | Civilian                       -- merchants, children -- killing these antagonizes all factions
	       deriving (Eq,Read,Show,Enum,Bounded)

factionPrefix :: Faction -> B.ByteString
factionPrefix Player = "Z"
factionPrefix InterstellarConcordance = "C"
factionPrefix PanGalacticTreatyOrganization = "P"
factionPrefix ImperialAlliance = "A"
factionPrefix Monsters = "M"
factionPrefix Pirates = "R"
factionPrefix Cyborgs = "Y"
factionPrefix SocialUtopiate = "U"
factionPrefix Whispers = "X"
factionPrefix Proselytes = "K"
factionPrefix Civilian = "Q"
