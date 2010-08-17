{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module DBData
    (Reference,
     toUID,
     CreatureRef,
     PlaneRef,
     ToolRef,
     BuildingRef,
     TheUniverse(..),
     the_universe,
     (=:=), (=/=),
     GenericReference(..),
     ReferenceType(..),
     LocationChild(..),
     LocationParent(..),
     Location,
     Position(..),
     Standing(..),
     Dropped(..),
     Inventory(..),
     Wielded(..),
     Constructed(..),
     Subsequent(..),
     Beneath(..),
     _nullary,
     _creature,
     _tool,
     _plane,
     _building,
     _standing,
     _dropped,
     _inventory,
     _wielded,
     _constructed,
     _subsequent,
     _position,
     _multiposition,
     _facing,
     _the_universe,
     asLocationTyped,
     asType,
     DBPrivate.S,
     location,
     entity,
     coerceReferenceTyped,
     isReferenceTyped,
     coerceLocationTyped,
     isLocationTyped,
     coerceEntityTyped,
     isEntityTyped,
     coerceLocationRecord,
     coerceLocation,
     coerceEntity,
     parent,
     child,
     generalizeLocation,
     generalizeEntity,
     generalizeLocationRecord,
     toStanding,
     toDropped,
     toInventory,
     toWielded,
     returnToInventory)
    where

import Facing
import DBPrivate
import ToolData
import CreatureData
import PlaneData
import BuildingData
import Data.Maybe
import Control.Monad
import Position

--
-- Type Instances
--
newtype Type a = Type a

_nullary :: Type (Reference ())
_nullary = Type $ error "_nullary: undefined"

_creature :: Type CreatureRef
_creature = Type $ error "_creature: undefined"

_tool :: Type ToolRef
_tool = Type $ error "_tool: undefined"

_plane :: Type PlaneRef
_plane = Type $ error "_plane: undefined"

_building :: Type BuildingRef
_building = Type $ error "_building: undefined"

_standing :: Type Standing
_standing = Type $ error "_standing: undefined"

_dropped :: Type Dropped
_dropped = Type $ error "_dropped: undefined"

_inventory :: Type Inventory
_inventory = Type $ error "_inventory: undefined"

_wielded :: Type Wielded
_wielded = Type $ error "_wielded: undefined"

_constructed :: Type Constructed
_constructed = Type $ error "_constructed: undefined"

_subsequent :: Type Subsequent
_subsequent = Type $ error "_subsequent: undefined"

_beneath :: Type Beneath
_beneath = Type $ error "_subsequent: undefined"

_position :: Type Position
_position = Type $ error "_position: undefined"

_multiposition :: Type MultiPosition
_multiposition = Type $ error "_multiposition: undefined"

_facing :: Type Facing
_facing = Type $ error "_facing: undefined"

_the_universe :: Type (Reference TheUniverse)
_the_universe = Type $ error "_the_universe: undefined"

--
-- Getting References generically.
--
class GenericReference a m | a -> m where
    fromLocation :: (ReferenceType x) => Location m (Reference x) b -> Maybe a
    generalizeReference :: a -> Reference ()

instance (GenericReference a m,GenericReference b m) => GenericReference (Either a b) m where
    fromLocation x = case (fromLocation x,fromLocation x) of
            (Just a,_) -> Just $ Left a
            (_,Just b) -> Just $ Right b
            _ | otherwise -> Nothing
    generalizeReference = either generalizeReference generalizeReference

instance (ReferenceType a) => GenericReference (Reference a) m where
    fromLocation = coerceReference . child
    generalizeReference = unsafeReference

instance (LocationChild c,LocationParent p) => GenericReference (Location m c p) m where
    fromLocation = coerceLocationRecord
    generalizeReference = child

--
-- Reference Equality
--
(=:=) :: (GenericReference a m,GenericReference b n) => a -> b -> Bool
a =:= b = generalizeReference a == generalizeReference b

(=/=) :: (GenericReference a m,GenericReference b n) => a -> b -> Bool
a =/= b = not $ a =:= b

--
-- References
--

the_universe :: Reference TheUniverse
the_universe = UniverseRef

coerceReferenceTyped :: (ReferenceType a) => Type (Reference a) -> Reference x -> Maybe (Reference a)
coerceReferenceTyped = const coerceReference

isReferenceTyped :: (ReferenceType a) => Type (Reference a) -> Reference x -> Bool
isReferenceTyped a = isJust . coerceReferenceTyped a

class ReferenceType a where
    coerceReference :: Reference x -> Maybe (Reference a)

instance ReferenceType () where
    coerceReference = Just . unsafeReference

instance ReferenceType Plane where
    coerceReference (PlaneRef ref) = Just $ PlaneRef ref
    coerceReference _ = Nothing

instance ReferenceType Tool where
    coerceReference (ToolRef ref) = Just $ ToolRef ref
    coerceReference _ = Nothing

instance ReferenceType Creature where
    coerceReference (CreatureRef ref) = Just $ CreatureRef ref
    coerceReference _ = Nothing

instance ReferenceType Building where
    coerceReference (BuildingRef ref) = Just $ BuildingRef ref
    coerceReference _ = Nothing

instance ReferenceType TheUniverse where
    coerceReference UniverseRef = Just UniverseRef
    coerceReference _ = Nothing

--
-- Locations
--
generalizeLocationRecord :: Location m e t -> Location m (Reference ()) ()
generalizeLocationRecord = unsafeLocation

generalizeLocation :: Location m e t -> Location m e ()
generalizeLocation = unsafeLocation

generalizeEntity :: Location m e t -> Location m (Reference ()) t
generalizeEntity = unsafeLocation

parent :: Location m e t -> Reference ()
parent (IsStanding _ s) = unsafeReference $ standing_plane s
parent (IsDropped _ d) = unsafeReference $ dropped_plane d
parent (InInventory _ c) = unsafeReference $ inventory_creature c
parent (IsWielded _ c) = unsafeReference $ wielded_creature c
parent (IsConstructed _ c) = unsafeReference $ constructed_plane c
parent (InTheUniverse _) = unsafeReference UniverseRef
parent (IsSubsequent _ b) = unsafeReference $ subsequent_to b
parent (IsBeneath _ b) = unsafeReference $ beneath_of b

child :: Location m e t -> Reference ()
child (IsStanding r _) = unsafeReference r
child (IsDropped r _) = unsafeReference r
child (InInventory r _) = unsafeReference r
child (IsWielded r _) = unsafeReference r
child (IsConstructed r _) = unsafeReference r
child (InTheUniverse r) = unsafeReference r
child (IsSubsequent r _) = unsafeReference r
child (IsBeneath r _) = unsafeReference r

asLocationTyped :: (LocationChild e,LocationParent t) => Type e -> Type t -> Location m e t -> Location m e t
asLocationTyped _ _ = id

asType :: Type e -> e -> e
asType _ = id

coerceLocationTyped :: (LocationChild c, LocationParent p) =>
                       Type p -> Location m c x -> Maybe (Location m c p)
coerceLocationTyped = const coerceLocation

isLocationTyped :: (LocationChild c,LocationParent p) => Type p -> Location m c x -> Bool
isLocationTyped t = isJust . coerceLocationTyped t

coerceEntityTyped :: (LocationChild c, LocationParent p) =>
                     Type c -> Location m x p -> Maybe (Location m c p)
coerceEntityTyped = const coerceEntity

isEntityTyped :: (LocationChild c, LocationParent p) => Type c -> Location m x p -> Bool
isEntityTyped t  = isJust . coerceEntityTyped t

coerceLocation :: (LocationChild e,LocationParent t) => Location m e x -> Maybe (Location m e t)
coerceLocation = coerceLocationRecord

coerceEntity :: (LocationChild e,LocationParent t) => Location m x t -> Maybe (Location m e t)
coerceEntity = coerceLocationRecord

coerceLocationRecord :: (LocationChild e,LocationParent t) => Location m x y -> Maybe (Location m e t)
coerceLocationRecord = fmap fst . coerceUnify
    where coerceUnify :: (LocationChild e,LocationParent t) =>
                             Location m x y -> Maybe (Location m e t,(e,t))
          coerceUnify l = do t <- extractParent l
                             e <- extractChild l
                             return (unsafeLocation l,(e,t))

location :: (LocationParent p) => Location m c p -> p
location l = fromMaybe (error "location: type error") $ extractParent l

entity :: (LocationChild c) => Location m c p -> c
entity l = fromMaybe (error "entity: type error") $ extractChild l

class (Eq a,Ord a) => LocationParent a where
    extractParent :: Location m e t -> Maybe a

class (Eq a,Ord a) => LocationChild a where
    extractChild :: Location m e t -> Maybe a

instance LocationParent Standing where
    extractParent (IsStanding _ s) = Just s
    extractParent _ = Nothing

instance LocationParent Dropped where
    extractParent (IsDropped _ d) = Just d
    extractParent _ = Nothing

instance LocationParent Inventory where
    extractParent (InInventory _ i) = Just i
    extractParent _ = Nothing

instance LocationParent Wielded where
    extractParent (IsWielded _ i) = Just i
    extractParent _ = Nothing

instance LocationParent Constructed where
    extractParent (IsConstructed _ i) = Just i
    extractParent _ = Nothing

instance LocationParent TheUniverse where
    extractParent (InTheUniverse {}) = Just TheUniverse
    extractParent _ = Nothing

instance LocationParent Subsequent where
    extractParent (IsSubsequent _ i) = Just i
    extractParent _ = Nothing

instance LocationParent Beneath where
    extractParent (IsBeneath _ i) = Just i
    extractParent _ = Nothing

instance LocationParent () where
    extractParent = const $ Just ()

instance LocationParent Position where
    extractParent (IsStanding _ s) = Just $ standing_position s
    extractParent (IsDropped _ d) = Just $ dropped_position d
    extractParent (InInventory {}) = Nothing
    extractParent (IsWielded {}) = Nothing
    extractParent (IsConstructed _ c) = Just $ constructed_position c
    extractParent (InTheUniverse {}) = Nothing
    extractParent (IsSubsequent {}) = Nothing
    extractParent (IsBeneath {}) = Nothing

instance LocationParent MultiPosition where
    extractParent (IsConstructed _ c) = Just $ multiPosition (constructed_position c) (buildingOccupies $ constructed_type c)
    extractParent x = fmap (toMultiPosition :: Position -> MultiPosition) $ extractParent x

instance LocationParent Facing where
    extractParent (IsStanding _ s) = Just $ standing_facing s
    extractParent (IsDropped {}) = Nothing
    extractParent (InInventory {}) = Nothing
    extractParent (IsWielded {}) = Nothing
    extractParent (IsConstructed {}) = Nothing
    extractParent (InTheUniverse {}) = Nothing
    extractParent (IsSubsequent {}) = Nothing
    extractParent (IsBeneath {}) = Nothing

instance ReferenceType a => LocationParent (Reference a) where
    extractParent = coerceReference . parent

instance ReferenceType a => LocationChild (Reference a) where
    extractChild = coerceReference . child

instance (LocationParent a,LocationParent b) => LocationParent (a,b) where
    extractParent l = liftM2 (,) (extractParent l) (extractParent l)

instance (LocationChild a,LocationChild b) => LocationChild (a,b) where
    extractChild l = liftM2 (,) (extractChild l) (extractChild l)

--
-- Manipulating Locations
--
toStanding :: (LocationParent t) =>
              Standing ->
              Location m CreatureRef t ->
              Location m CreatureRef Standing
toStanding s l | isEntityTyped _creature l = IsStanding (entity l) s
toStanding _ _ = error "toStanding: type error"

toDropped :: (LocationParent t) => Dropped -> Location m ToolRef t -> Location m ToolRef Dropped
toDropped d l | isEntityTyped _tool l = IsDropped (entity l) d
toDropped _ _ = error "toDropped: type error"

toInventory :: (LocationParent t) => Inventory -> Location m ToolRef t -> Location m ToolRef Inventory
toInventory i l | isEntityTyped _tool l = InInventory (entity l) i
toInventory _ _ = error "toInventory: type error"

toWielded :: (LocationParent t) => Wielded -> Location m ToolRef t -> Location m ToolRef Wielded
toWielded i l | isEntityTyped _tool l = IsWielded (entity l) i
toWielded _ _ = error "toWielded: type error"

returnToInventory :: Location m ToolRef Wielded -> Location m ToolRef Inventory
returnToInventory l = InInventory (entity l) (Inventory c)
    where Wielded c = location l

