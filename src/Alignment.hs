module Alignment
    (Alignment,
     AlignmentEthic(Lawful,Neutral,Chaotic,Evil),
     AlignmentSchool(Strategic,Tactical,Diplomatic,Indifferent),
     AlignmentStrength(Strong,Moderate,Weak),
     modifierFromAlignment, canSmiteAlignment,
     canMassiveSmiteAlignment, canLethalSmiteAlignment,
     smiteDamage
    )
    where

import StatsData
--
-- The alignment system.
-- 

data AlignmentEthic = Lawful | Neutral | Chaotic | Evil
data AlignmentSchool = Strategic | Tactical | Diplomatic | Indifferent
data AlignmentStrength = Strong | Moderate |  Weak
type Alignment = (AlignmentStrength,AlignmentEthic,AlignmentSchool)

-- |
-- Generates an enhancement according to alignment and stats.  Alignments are associated
-- with different statistics (Lawful with Strength, Neutral with Constitution, Chaotic with Dexterity,
-- Strategic with Intelligence, Tactical with Perception, Diplomatic with Charsma, and Indifferent with
-- mindfulness.  Evil is associated with all of the mental alignments.)
--
modifierFromAlignment :: Alignment -> Stats -> Integer
modifierFromAlignment (strength, ethic, schl) sts =
    limitModifierByAlignmentStrength strength (modifierFromAlignmentEthic ethic sts + modifierFromAlignmentSchool schl sts)

-- |
-- Helpers for modifierFromAlignment
--
modifierFromAlignmentEthic :: AlignmentEthic -> Stats -> Integer

modifierFromAlignmentEthic Lawful sts = max 0 (3*(str sts))
modifierFromAlignmentEthic Neutral sts = max 0 (con sts)
modifierFromAlignmentEthic Chaotic sts = max 0 (2*(dex sts))
modifierFromAlignmentEthic Evil sts = max 0 $ (int sts) + (per sts) + (cha sts) + (mind sts)

modifierFromAlignmentSchool :: AlignmentSchool -> Stats -> Integer
modifierFromAlignmentSchool Strategic = (max 0) . int
modifierFromAlignmentSchool Tactical = (max 0) . per
modifierFromAlignmentSchool Diplomatic = (max 0) . cha
modifierFromAlignmentSchool Indifferent = (max 0) . mind

limitModifierByAlignmentStrength :: AlignmentStrength -> Integer -> Integer
limitModifierByAlignmentStrength Weak stat = min 1 (stat `quot` 3)
limitModifierByAlignmentStrength Moderate stat = min 5 (stat `quot` 2)
limitModifierByAlignmentStrength Strong stat = stat

-- |
-- Answers true if the first specified alignment can make a smite attack against the second.
--
canSmiteAlignment :: Alignment -> Alignment -> Bool

canSmiteAlignment _ (_,_,Indifferent) = False -- no one can smite an indifferent creature
canSmiteAlignment (_,_,Indifferent) _ = False -- indifferent alignments can't smite

canSmiteAlignment (_,_,Strategic) (_,_,Tactical) = False -- strategic alignments don't tend to see individuals as threats
canSmiteAlignment (_,_,Tactical) (_,_,Diplomatic) = False -- you can't kill an idea

canSmiteAlignment (Weak,_,_) (Moderate,_,_) = False  -- weaker alignments can't smite stronger alignments
canSmiteAlignment (Weak,_,_) (Strong,_,_) = False
canSmiteAlignment (Moderate,_,_) (Strong,_,_) = False

canSmiteAlignment (_,Evil,_) (_,Lawful,_) = True  -- evil alignments (even weak evil alignments) can smite all others
canSmiteAlignment (_,Evil,_) (_,Neutral,_) = True
canSmiteAlignment (_,Evil,_) (_,Chaotic,_) = True

canSmiteAlignment (_,_,Strategic) (_,Evil,_) = True -- only strategic alignments can smite evil

canSmiteAlignment (_,Lawful,_) (_,Chaotic,_) = True -- order overpowers chaos (in direct conflict)
canSmiteAlignment (_,Neutral,_) (_,Lawful,_) = True -- pragmatism overpowers idealism
canSmiteAlignment (_,Chaotic,_) (_,Neutral,_) = True -- chaos preys on the commoner

canSmiteAlignment _ _ = False

-- |
-- Returns true if the first alignment can make a massive smite against the second.  A massive smite does additional damage.
--
canMassiveSmiteAlignment :: Alignment -> Alignment -> Bool

canMassiveSmiteAlignment align1@(Strong,_,_) align2@(Moderate,_,_) = canSmiteAlignment align1 align2
canMassiveSmiteAlignment align1@(Strong,_,_) align2@(Weak,_,_) = canSmiteAlignment align1 align2
canMassiveSmiteAlignment align1@(Moderate,_,_) align2@(Weak,_,_) = canSmiteAlignment align1 align2

canMassiveSmiteAlignment _ _ = False

-- |
-- Returns true if the first alignment can make a lethal smite against the second.  A lethal smite will usually kill.
--
canLethalSmiteAlignment :: Alignment -> Alignment -> Bool

canLethalSmiteAlignment align1@(Strong,_,_) align2@(Weak,_,_) = canSmiteAlignment align1 align2

canLethalSmiteAlignment _ _ = False

-- |
-- Returns the damage if the first alignment smites the second, based on the stats of the first alignment.
--
smiteDamage :: Alignment -> Stats -> Alignment -> Integer

smiteDamage align1 sts align2 = if (canLethalSmiteAlignment align1 align2) then (modifierFromAlignment align1 sts)^2
                                else if (canMassiveSmiteAlignment align1 align2) then 2*(modifierFromAlignment align1 sts)
				else if (canSmiteAlignment align1 align2) then (modifierFromAlignment align1 sts)
				else 0