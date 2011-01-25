
module CreatureData
    (Creature(..),
     CreatureGender(..),
     CreatureAptitude(..),
     CreatureInteractionMode(..),
     CreatureAbility(..),
     CreatureEndo(..),
     CreatureScore(..),
     FavoredClass(..),
     creatureGender,
     creatureAbilityScore,
     isFavoredClass,
     empty_creature)
    where

import CharacterData
import Alignment
import Data.Maybe
import FactionData
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import SpeciesData
import TerrainData

data Creature = Creature { creature_aptitude :: Map.Map CreatureAptitude Integer,
                           creature_ability :: Map.Map CreatureAbility Integer,
                           creature_ethical :: Map.Map EthicalAlignment Integer,
                           creature_levels :: Map.Map CharacterClass Integer,
                           creature_favored_classes :: Set.Set CharacterClass,
                           creature_gender :: CreatureGender,
                           creature_species :: Species,
                           creature_random_id :: Integer, -- random number attached to the creature, not unique
                           creature_damage :: Integer,
                           creature_faction :: Faction,
                           creature_points :: Integer }
                                deriving (Read,Show)

-- | Creature having no attributes and undefined 'creature_species', 'creature_random_id', and 'creature_faction'
--
empty_creature :: Creature
empty_creature = Creature {
    creature_aptitude = Map.empty,
    creature_ability = Map.empty,
    creature_ethical = Map.empty,
    creature_levels = Map.empty,
    creature_favored_classes = Set.empty,
    creature_gender = Neuter,
    creature_species = error "empty_creature: undefined creature_species",
    creature_random_id = error "empty_creature: undefined creature_random_id",
    creature_damage = 0,
    creature_faction = error "empty_creature: undefined creature_faction",
    creature_points = 0 }

data CreatureGender = Male | Female | Neuter deriving (Eq,Read,Show)

-- | Endomorphisms over a 'Creature'.  These are types that contribute some feature to a 'Creature',
-- so that 'Creature's can be defined concisely by those properties.
class CreatureEndo a where
    applyToCreature :: a -> Creature -> Creature

-- | Primitive numeric properties of a Creature.
class CreatureScore s where
    rawScore :: s -> Creature -> Integer

instance (CreatureEndo a,Integral i) => CreatureEndo (a,i) where
    applyToCreature (_,i) | i <= 0 = id
    applyToCreature (a,i) = applyToCreature (a,toInteger i - 1) . applyToCreature a

instance (CreatureEndo a) => CreatureEndo [a] where
    applyToCreature = appEndo . mconcat . map (Endo . applyToCreature)

instance CreatureEndo CreatureGender where
    applyToCreature g c = c { creature_gender = g }

-- | The seven aptitudes.
data CreatureAptitude =
     Strength
   | Speed
   | Constitution
   | Intellect
   | Perception
   | Charisma
   | Mindfulness
         deriving (Eq,Read,Show,Ord,Enum,Bounded)

instance CreatureEndo CreatureAptitude where
    applyToCreature aptitude c = c { creature_aptitude = Map.insertWith (+) aptitude 1 $ creature_aptitude c }

instance CreatureScore CreatureAptitude where
    rawScore aptitude c = fromMaybe 0 $ Map.lookup aptitude (creature_aptitude c)

-- | Combat modes:
-- Melee is armed close-quarters combat with bladed or blunt weapons
-- Ranged is combat with projectile weapons
-- Unarmed is close-quarters hand-to-hand
-- Splash represts diffuse damage caused by things like explosions or falling into lava.
data CreatureInteractionMode = Melee | Ranged | Unarmed | Splash
    deriving (Eq,Read,Show,Ord)

data CreatureAbility =
     ToughnessTrait
   | AttackSkill CreatureInteractionMode
   | DefenseSkill CreatureInteractionMode
   | DamageSkill CreatureInteractionMode
   | DamageReductionTrait CreatureInteractionMode
   | ReloadSkill CreatureInteractionMode
   | TerrainAffinity TerrainPatch
   | HideSkill
   | SpotSkill
   | JumpSkill
   | InventorySkill
         deriving (Eq,Read,Show,Ord)

instance CreatureEndo CreatureAbility where
    applyToCreature ability c = c { creature_ability = Map.insertWith (+) ability 1 $ creature_ability c }

instance CreatureScore CreatureAbility where
    rawScore ability c = fromMaybe 0 $ Map.lookup ability $ creature_ability c

instance CreatureEndo EthicalAlignment where
    applyToCreature ethical c = c { creature_ethical = Map.insertWith (+) ethical 1 $ creature_ethical c }

instance CreatureScore EthicalAlignment where
    rawScore ethical c = fromMaybe 0 $ Map.lookup ethical $ creature_ethical c

instance CreatureEndo CharacterClass where
    applyToCreature character_class c = c { creature_levels = Map.insertWith (+) character_class 1 $ creature_levels c }

instance CreatureScore CharacterClass where
    rawScore character_class c = fromMaybe 0 $ Map.lookup character_class $ creature_levels c

newtype FavoredClass = FavoredClass CharacterClass

instance CreatureEndo FavoredClass where
    applyToCreature (FavoredClass favored_class) c = c { creature_favored_classes = Set.insert favored_class $ creature_favored_classes c }

-- | Calculator to determine how many ranks a creature has in an ability.
-- Number of aptitude points plus n times number of ability points
figureAbility :: [CreatureAptitude] -> (CreatureAbility,Integer) -> Creature -> Integer
figureAbility aptitude (ability,n) c = sum (map (flip rawScore c) aptitude) + rawScore ability c * n

creatureAbilityScore :: CreatureAbility -> Creature -> Integer
creatureAbilityScore ToughnessTrait = figureAbility [Strength,Speed,Constitution,Mindfulness] (ToughnessTrait,3)
creatureAbilityScore (AttackSkill Melee) = figureAbility [Strength] (AttackSkill Melee,2)
creatureAbilityScore (DefenseSkill Melee) = figureAbility [Strength] (DefenseSkill Melee,2)
creatureAbilityScore (DamageSkill Melee) = figureAbility [Strength] (DamageSkill Melee,2)
creatureAbilityScore (DamageReductionTrait Melee) = figureAbility [Constitution] (DamageReductionTrait Melee,1)
creatureAbilityScore (ReloadSkill Melee) = figureAbility [Speed] (ReloadSkill Melee,1)
creatureAbilityScore (AttackSkill Ranged) = figureAbility [Perception] (AttackSkill Ranged,2)
creatureAbilityScore (DefenseSkill Ranged) = figureAbility [Perception] (DefenseSkill Ranged,2)
creatureAbilityScore (DamageSkill Ranged) = figureAbility [Perception] (DamageSkill Ranged,2)
creatureAbilityScore (DamageReductionTrait Ranged) = figureAbility [Constitution] (DamageReductionTrait Ranged,1)
creatureAbilityScore (ReloadSkill Ranged) = figureAbility [Speed] (ReloadSkill Ranged,1)
creatureAbilityScore (AttackSkill Unarmed) = figureAbility [Speed] (AttackSkill Unarmed,2)
creatureAbilityScore (DefenseSkill Unarmed) = figureAbility [Speed] (DefenseSkill Unarmed,2)
creatureAbilityScore (DamageSkill Unarmed) = figureAbility [Speed] (DamageSkill Unarmed,2)
creatureAbilityScore (DamageReductionTrait Unarmed) = figureAbility [Constitution] (DamageReductionTrait Unarmed,1)
creatureAbilityScore (ReloadSkill Unarmed) = figureAbility [Speed] (ReloadSkill Unarmed,1)
creatureAbilityScore (AttackSkill Splash) = figureAbility [Intellect] (AttackSkill Splash,2)
creatureAbilityScore (DefenseSkill Splash) = figureAbility [Intellect] (DefenseSkill Splash,2)
creatureAbilityScore (DamageSkill Splash) = figureAbility [Intellect] (DamageSkill Splash,2)
creatureAbilityScore (DamageReductionTrait Splash) = figureAbility [Constitution] (DamageReductionTrait Splash,1)
creatureAbilityScore (ReloadSkill Splash) = figureAbility [Speed] (ReloadSkill Splash,1)
creatureAbilityScore (TerrainAffinity terrain_type) = figureAbility [] (TerrainAffinity terrain_type,1)
creatureAbilityScore HideSkill = figureAbility [Perception] (HideSkill,2)
creatureAbilityScore SpotSkill = figureAbility [Perception] (SpotSkill,2)
creatureAbilityScore JumpSkill = figureAbility [Strength] (JumpSkill,2)
creatureAbilityScore InventorySkill = figureAbility [Strength,Speed,Constitution] (InventorySkill,2)

-- |
-- Answers the gender of this creature.
--
creatureGender :: Creature -> CreatureGender
creatureGender = creature_gender

-- |
-- Answers true if the specified class is a favored class for this creature.
--
isFavoredClass :: CharacterClass -> Creature -> Bool
isFavoredClass character_class creature = character_class `Set.member` (creature_favored_classes creature)

