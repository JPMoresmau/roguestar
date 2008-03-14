
module DBData
    (Reference,
     toUID,
     CreatureRef,
     PlaneRef,
     ToolRef,
     TheUniverse(..),
     ReferenceType(..),
     LocationType(..),
     Location,
     Position(..),
     Standing(..),
     Dropped(..),
     Inventory(..),
     DBPrivate.S,
     genericReference,
     genericChild,
     genericParent,
     genericLocation,
     genericLocationP,
     genericLocationC,
     standCreature,
     dropTool,
     pickupTool,
     entity,
     location,
     coerceLocation,
     coerceParent,
     coerceChild,
     isCreatureRef,
     isPlaneRef,
     isToolRef,
     isTheUniverse,
     theUniverse,
     toCreatureRef,
     toPlaneRef,
     toToolRef,
     isCreatureLocation,
     toCreatureLocation,
     isToolLocation,
     toToolLocation,
     isPlaneLocation,
     toPlaneLocation,
     isStandingLocation,
     toStandingLocation,
     isDroppedLocation,
     toDroppedLocation,
     isInventoryLocation,
     toInventoryLocation,
     isPlanarLocation,
     toPlanarLocation,
     isCarriedLocation,
     toCarriedLocation,
     position,
     isPositionLocation,
     toPositionLocation,
     facing,
     isFacingLocation,
     toFacingLocation)
    where

import Facing
import DBPrivate
import ToolData
import CreatureData
import PlaneData
import Data.Maybe
import Control.Monad

isCreatureRef :: Reference a -> Bool
isCreatureRef = isJust . toCreatureRef

isPlaneRef :: Reference a -> Bool
isPlaneRef = isJust . toPlaneRef

isToolRef :: Reference a -> Bool
isToolRef = isJust . toToolRef

isTheUniverse :: Reference a -> Bool
isTheUniverse UniverseRef = True
isTheUniverse _ = False

theUniverse :: Reference TheUniverse
theUniverse = UniverseRef

toToolRef :: Reference a -> Maybe (Reference Tool)
toToolRef (ToolRef x) = Just $ ToolRef x
toToolRef _ = Nothing

toCreatureRef :: Reference a -> Maybe (Reference Creature)
toCreatureRef (CreatureRef x) = Just $ CreatureRef x
toCreatureRef _ = Nothing

toPlaneRef :: Reference a -> Maybe (Reference Plane)
toPlaneRef (PlaneRef x) = Just $ PlaneRef x
toPlaneRef _ = Nothing

genericReference :: Reference a -> Reference ()
genericReference = unsafeReference

class ReferenceType a where
    coerceReference :: Reference x -> Maybe (Reference a)

instance ReferenceType () where
    coerceReference = Just . unsafeReference

instance ReferenceType Plane where
    coerceReference = toPlaneRef

instance ReferenceType Tool where
    coerceReference = toToolRef

instance ReferenceType Creature where
    coerceReference = toCreatureRef

instance ReferenceType TheUniverse where
    coerceReference (UniverseRef) = Just UniverseRef
    coerceReference _ = Nothing

standCreature :: Standing -> Location m CreatureRef t -> Location m CreatureRef Standing
standCreature s l | isCreatureLocation l = IsStanding (entity l) s
standCreature _ _ = error "stand: type error"

dropTool :: Dropped -> Location m ToolRef t -> Location m ToolRef Dropped
dropTool d l | isToolLocation l = IsDropped (entity l) d
dropTool _ _ = error "drop: type error"

pickupTool :: Inventory -> Location m ToolRef t -> Location m ToolRef Inventory
pickupTool i l | isToolLocation l = InInventory (entity l) i
pickupTool _ _ = error "pickup: type error"

genericLocation :: Location m e t -> Location m () ()
genericLocation = unsafeLocation

genericLocationP :: Location m e t -> Location m e ()
genericLocationP = unsafeLocation

genericLocationC :: Location m e t -> Location m () t
genericLocationC = unsafeLocation

genericParent :: Location m e t -> Reference ()
genericParent (IsStanding _ s) = unsafeReference $ standing_plane s
genericParent (IsDropped _ d) = unsafeReference $ dropped_plane d
genericParent (InInventory _ c) = unsafeReference $ inventory_creature c
genericParent (InTheUniverse _) = unsafeReference UniverseRef

genericChild :: Location m e t -> Reference ()
genericChild (IsStanding r _) = unsafeReference r
genericChild (IsDropped r _) = unsafeReference r
genericChild (InInventory r _) = unsafeReference r
genericChild (InTheUniverse r) = unsafeReference r

isCreatureLocation :: (LocationType e) => Location m e t -> Bool
isCreatureLocation = isCreatureRef . genericChild

isToolLocation :: (LocationType e) => Location m e t -> Bool
isToolLocation = isToolRef . genericChild

isPlaneLocation :: (LocationType e) => Location m e t -> Bool
isPlaneLocation = isPlaneRef . genericChild

isStandingLocation :: (LocationType e) => Location m e t -> Bool
isStandingLocation = isJust . toStandingLocation

isDroppedLocation :: (LocationType e) => Location m e t -> Bool
isDroppedLocation = isJust . toDroppedLocation

isInventoryLocation :: (LocationType e) => Location m e t -> Bool
isInventoryLocation = isJust . toInventoryLocation

toCreatureLocation :: (LocationType t) => Location m e t -> Maybe (Location m CreatureRef t)
toCreatureLocation = coerceLocation

toToolLocation :: (LocationType t) => Location m e t -> Maybe (Location m ToolRef t)
toToolLocation = coerceLocation

toPlaneLocation :: (LocationType t) => Location m e t -> Maybe (Location m PlaneRef t)
toPlaneLocation = coerceLocation

toStandingLocation :: (LocationType e) => Location m e t -> Maybe (Location m e Standing)
toStandingLocation = coerceLocation

toDroppedLocation :: (LocationType e) => Location m e t -> Maybe (Location m e Dropped)
toDroppedLocation = coerceLocation

toInventoryLocation :: (LocationType e) => Location m e t -> Maybe (Location m e Inventory)
toInventoryLocation = coerceLocation

coerceParent :: (LocationType e,LocationType t) => Location m e x -> Maybe (Location m e t)
coerceParent = coerceLocation

coerceChild :: (LocationType e,LocationType t) => Location m e x -> Maybe (Location m e t)
coerceChild = coerceLocation

coerceLocation :: (LocationType e,LocationType t) => Location m x y -> Maybe (Location m e t)
coerceLocation = fmap fst . coerceUnify
    where coerceUnify :: (LocationType e,LocationType t) => 
                             Location m x y -> Maybe (Location m e t,(e,t))
          coerceUnify l = do t <- extractLocation l
                             e <- extractChild l
                             return (unsafeLocation l,(e,t))

location :: (LocationType t) => Location m e t -> t
location l = fromMaybe (error "location: type error") $ extractLocation l

entity :: (LocationType e) => Location m e t -> e
entity l = fromMaybe (error "entity: type error") $ extractChild l

class LocationType a where
    extractLocation :: Location m e t -> Maybe a
    extractChild :: Location m e t -> Maybe a

instance LocationType Standing where
    extractLocation (IsStanding _ s) = Just s
    extractLocation _ = Nothing
    extractChild = const Nothing

instance LocationType Dropped where
    extractLocation (IsDropped _ d) = Just d
    extractLocation _ = Nothing
    extractChild = const Nothing

instance LocationType Inventory where
    extractLocation (InInventory _ i) = Just i
    extractLocation _ = Nothing
    extractChild = const Nothing

instance LocationType Facing where
    extractLocation = facing
    extractChild = const Nothing

instance LocationType Position where
    extractLocation = position
    extractChild = const Nothing

instance LocationType () where
    extractLocation = const $ Just ()
    extractChild = const $ Just ()

instance ReferenceType a => LocationType (Reference a) where
    extractLocation = coerceReference . genericParent
    extractChild = coerceReference . genericChild

instance (LocationType a,LocationType b) => LocationType (a,b) where
    extractLocation l = liftM2 (,) (extractLocation l) (extractLocation l)
    extractChild l = liftM2 (,) (extractChild l) (extractChild l)

position :: Location m e t -> Maybe Position
position (IsStanding _ s) = Just $ standing_position s
position (IsDropped _ d) = Just $ dropped_position d
position (InInventory {}) = Nothing
position (InTheUniverse {}) = Nothing

facing :: Location m e t -> Maybe Facing
facing (IsStanding _ s) = Just $ standing_facing s
facing (IsDropped {}) = Nothing
facing (InInventory {}) = Nothing
facing (InTheUniverse {}) = Nothing

isFacingLocation :: Location m e t -> Bool
isFacingLocation = isJust . facing

toFacingLocation :: (LocationType e) => Location m e t -> Maybe (Location m e Facing)
toFacingLocation = coerceLocation

isPositionLocation :: Location m e t -> Bool
isPositionLocation = isJust . position

toPositionLocation :: (LocationType e) => Location m e t -> Maybe (Location m e Position)
toPositionLocation = coerceLocation

isPlanarLocation :: Location m e t -> Bool
isPlanarLocation = isPlaneRef . genericParent

toPlanarLocation :: (LocationType e) => Location m e t -> Maybe (Location m e PlaneRef)
toPlanarLocation = coerceLocation

isCarriedLocation :: Location m e t -> Bool
isCarriedLocation = isCreatureRef . genericParent

toCarriedLocation :: (LocationType e) => Location m e t -> Maybe (Location m e CreatureRef)
toCarriedLocation = coerceLocation
