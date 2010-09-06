{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             FunctionalDependencies,
             UndecidableInstances,
             ScopedTypeVariables #-}

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
     Type,
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
     _beneath,
     _position,
     _multiposition,
     _facing,
     _the_universe,
     asLocationTyped,
     asType,
     parent,
     child,
     coerceReferenceTyped,
     isReferenceTyped,
     coerceParentTyped,
     isParentTyped,
     coerceChildTyped,
     isChildTyped,
     coerceLocationRecord,
     coerceParent,
     coerceChild,
     genericParent,
     genericChild,
     generalizeParent,
     generalizeChild,
     generalizeLocation,
     toStanding,
     toDropped,
     toInventory,
     toWielded,
     returnToInventory,
     shuntToTheUniverse)
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
class GenericReference a where
    fromLocation :: (ReferenceType x) => Location (Reference x) b -> Maybe a
    generalizeReference :: a -> Reference ()

instance (GenericReference a,GenericReference b) => GenericReference (Either a b) where
    fromLocation x = case (fromLocation x,fromLocation x) of
            (Just a,_) -> Just $ Left a
            (_,Just b) -> Just $ Right b
            _ | otherwise -> Nothing
    generalizeReference = either generalizeReference generalizeReference

instance (ReferenceType a) => GenericReference (Reference a) where
    fromLocation = coerceReference . genericChild
    generalizeReference = unsafeReference

instance (LocationChild c,LocationParent p) => GenericReference (Location c p) where
    fromLocation = coerceLocationRecord
    generalizeReference = genericChild

--
-- Reference Equality
--
(=:=) :: (GenericReference a,GenericReference b) => a -> b -> Bool
a =:= b = generalizeReference a == generalizeReference b

(=/=) :: (GenericReference a,GenericReference b) => a -> b -> Bool
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
generalizeLocation :: Location e t -> Location (Reference ()) ()
generalizeLocation = unsafeLocation

generalizeParent :: Location e t -> Location e ()
generalizeParent = unsafeLocation

generalizeChild :: Location e t -> Location (Reference ()) t
generalizeChild = unsafeLocation

genericParent :: Location e t -> Reference ()
genericParent (IsStanding _ s) = unsafeReference $ standing_plane s
genericParent (IsDropped _ d) = unsafeReference $ dropped_plane d
genericParent (InInventory _ c) = unsafeReference $ inventory_creature c
genericParent (IsWielded _ c) = unsafeReference $ wielded_creature c
genericParent (IsConstructed _ c) = unsafeReference $ constructed_plane c
genericParent (InTheUniverse _) = unsafeReference UniverseRef
genericParent (IsSubsequent _ b) = unsafeReference $ subsequent_to b
genericParent (IsBeneath _ b) = unsafeReference $ beneath_of b

genericChild :: Location e t -> Reference ()
genericChild (IsStanding r _) = unsafeReference r
genericChild (IsDropped r _) = unsafeReference r
genericChild (InInventory r _) = unsafeReference r
genericChild (IsWielded r _) = unsafeReference r
genericChild (IsConstructed r _) = unsafeReference r
genericChild (InTheUniverse r) = unsafeReference r
genericChild (IsSubsequent r _) = unsafeReference r
genericChild (IsBeneath r _) = unsafeReference r

asLocationTyped :: (LocationChild e,LocationParent t) =>
                   Type e -> Type t -> Location e t -> Location e t
asLocationTyped _ _ = id

asType :: Type e -> e -> e
asType _ = id

coerceParentTyped :: (LocationParent p) =>
                       Type p -> Location c x -> Maybe (Location c p)
coerceParentTyped = const coerceParent

isParentTyped :: (LocationParent p) => Type p -> Location c x -> Bool
isParentTyped t = isJust . coerceParentTyped t

coerceChildTyped :: (LocationChild c) =>
                     Type c -> Location x p -> Maybe (Location c p)
coerceChildTyped = const coerceChild

isChildTyped :: (LocationChild c) => Type c -> Location x p -> Bool
isChildTyped t  = isJust . coerceChildTyped t

coerceParent :: forall c x p. (LocationParent p) =>
                Location c x -> Maybe (Location c p)
coerceParent l =
    do (_ :: p) <- extractParent l
       return $ unsafeLocation l

coerceChild :: forall x p c. (LocationChild c) =>
                Location x p -> Maybe (Location c p)
coerceChild l =
    do (_ :: c) <- extractChild l
       return $ unsafeLocation l

coerceLocationRecord :: forall x y c p. (LocationChild c,LocationParent p) =>
                        Location x y -> Maybe (Location c p)
coerceLocationRecord l =
    do (_ :: p) <- extractParent l
       (_ :: c) <- extractChild l
       return $ unsafeLocation l

parent :: (LocationParent p) => Location c p -> p
parent l = fromMaybe (error "parent: type error") $ extractParent l

child :: (LocationChild c) => Location c p -> c
child l = fromMaybe (error "child: type error") $ extractChild l

class (Eq a,Ord a) => LocationParent a where
    extractParent :: Location e t -> Maybe a

class (Eq a,Ord a) => LocationChild a where
    extractChild :: Location e t -> Maybe a

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
    extractParent = coerceReference . genericParent

instance ReferenceType a => LocationChild (Reference a) where
    extractChild = coerceReference . genericChild

instance (LocationParent a,LocationParent b) => LocationParent (a,b) where
    extractParent l = liftM2 (,) (extractParent l) (extractParent l)

instance (LocationChild a,LocationChild b) => LocationChild (a,b) where
    extractChild l = liftM2 (,) (extractChild l) (extractChild l)

--
-- Manipulating Locations
--
toStanding :: (LocationParent t) =>
              Standing ->
              Location CreatureRef t ->
              Location CreatureRef Standing
toStanding s l | isChildTyped _creature l = IsStanding (child l) s
toStanding _ _ = error "toStanding: type error"

toDropped :: (LocationParent t) => Dropped -> Location ToolRef t -> Location ToolRef Dropped
toDropped d l | isChildTyped _tool l = IsDropped (child l) d
toDropped _ _ = error "toDropped: type error"

toInventory :: (LocationParent t) => Inventory -> Location ToolRef t -> Location ToolRef Inventory
toInventory i l | isChildTyped _tool l = InInventory (child l) i
toInventory _ _ = error "toInventory: type error"

toWielded :: (LocationParent t) => Wielded -> Location ToolRef t -> Location ToolRef Wielded
toWielded i l | isChildTyped _tool l = IsWielded (child l) i
toWielded _ _ = error "toWielded: type error"

returnToInventory :: Location ToolRef Wielded -> Location ToolRef Inventory
returnToInventory l = InInventory (child l) (Inventory c)
    where Wielded c = parent l

shuntToTheUniverse :: Type p ->
                      Location PlaneRef p ->
                      Location PlaneRef TheUniverse
shuntToTheUniverse _ l = InTheUniverse (child l)

