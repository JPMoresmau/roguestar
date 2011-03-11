module PlayerState
    (PlayerState(..),
     CreatureTurnMode(..),
     SnapshotEvent(..),
     creatureOf,
     subjectOf,
     menuIndex,
     modifyMenuIndex)
    where

import DBData
import CreatureData
import CharacterData
import MakeData
import TravelData

data PlayerState =
    SpeciesSelectionState
  | ClassSelectionState Creature
  | PlayerCreatureTurn CreatureRef CreatureTurnMode
  | SnapshotEvent SnapshotEvent
  | GameOver
     deriving (Read,Show)

data CreatureTurnMode =
    NormalMode
  | MoveMode
  | PickupMode Integer
  | DropMode Integer
  | WieldMode Integer
  | MakeMode Integer PrepareMake
  | AttackMode
  | FireMode
  | JumpMode
  | TurnMode
  | ClearTerrainMode
      deriving (Read,Show)

data SnapshotEvent =
    AttackEvent {
        attack_event_source_creature :: CreatureRef,
        attack_event_source_weapon :: Maybe ToolRef,
        attack_event_target_creature :: CreatureRef }
  | MissEvent {
        miss_event_creature :: CreatureRef,
        miss_event_weapon :: Maybe ToolRef }
  | KilledEvent {
        killed_event_creature :: CreatureRef }
  | WeaponOverheatsEvent {
        weapon_overheats_event_creature :: CreatureRef,
        weapon_overheats_event_weapon :: ToolRef }
  | WeaponExplodesEvent {
        weapon_explodes_event_creature :: CreatureRef,
        weapon_explodes_event_weapon :: ToolRef }
  | DisarmEvent {
        disarm_event_source_creature :: CreatureRef,
        disarm_event_target_creature :: CreatureRef,
        disarm_event_target_tool :: ToolRef }
  | SunderEvent {
        sunder_event_source_creature :: CreatureRef,
        sunder_event_source_weapon :: ToolRef,
        sunder_event_target_creature :: CreatureRef,
        sunder_event_target_tool :: ToolRef }
  | TeleportEvent {
        teleport_event_creature :: CreatureRef }
  | ClimbEvent {
        climb_event_direction :: ClimbDirection,
        climb_event_creature :: CreatureRef }
  | HealEvent {
        heal_event_creature :: CreatureRef }
  | ExpendToolEvent {
        expend_tool_event_tool :: ToolRef }
  | BumpEvent {
        bump_event_creature :: CreatureRef,
        bump_event_new_level :: Maybe Integer,
        bump_event_new_class :: Maybe CharacterClass }
            deriving (Read,Show)

-- | Get the 'Creature' acting in the given 'PlayerState'.
creatureOf :: PlayerState -> Maybe CreatureRef
creatureOf state = case state of
    PlayerCreatureTurn creature_ref _ -> Just creature_ref
    SnapshotEvent event -> subjectOf event
    GameOver -> Nothing
    ClassSelectionState {} -> Nothing
    SpeciesSelectionState {} -> Nothing

-- | Get the subject creature of a 'SnapshotEvent', that is, the creature taking action.
subjectOf :: SnapshotEvent -> Maybe CreatureRef
subjectOf event = case event of
    AttackEvent { attack_event_source_creature = attacker_ref } -> Just attacker_ref
    MissEvent { miss_event_creature = attacker_ref } -> Just attacker_ref
    WeaponOverheatsEvent { weapon_overheats_event_creature = attacker_ref } -> Just attacker_ref
    WeaponExplodesEvent { weapon_explodes_event_creature = attacker_ref } -> Just attacker_ref
    KilledEvent killed_ref -> Just killed_ref
    DisarmEvent { disarm_event_source_creature = attacker_ref } -> Just attacker_ref
    SunderEvent { sunder_event_source_creature = attacker_ref } -> Just attacker_ref
    TeleportEvent { teleport_event_creature = creature_ref } -> Just creature_ref
    HealEvent { heal_event_creature = creature_ref } -> Just creature_ref
    ClimbEvent { climb_event_creature = creature_ref } -> Just creature_ref
    BumpEvent { bump_event_creature = creature_ref } -> Just creature_ref
    ExpendToolEvent {} -> Nothing

-- | Current index into the menu, if there is one.
menuIndex :: PlayerState -> Maybe Integer
menuIndex state = fst $ modifyMenuIndex_ id state

-- | Modify the current index into the menu, if there is one (otherwise has no effect).
modifyMenuIndex :: (Integer -> Integer) -> PlayerState -> PlayerState
modifyMenuIndex f state = snd $ modifyMenuIndex_ f state

modifyMenuIndex_ :: (Integer -> Integer) -> PlayerState -> (Maybe Integer,PlayerState)
modifyMenuIndex_ f state = case state of
    PlayerCreatureTurn c (PickupMode n) -> (Just n,PlayerCreatureTurn c $ PickupMode $ f n)
    PlayerCreatureTurn c (DropMode n) -> (Just n,PlayerCreatureTurn c $ DropMode $ f n)
    PlayerCreatureTurn c (WieldMode n) -> (Just n,PlayerCreatureTurn c $ WieldMode $ f n)
    PlayerCreatureTurn c (MakeMode n make_prep) -> (Just n,PlayerCreatureTurn c $ MakeMode (f n) make_prep)
    x -> (Nothing,x)

