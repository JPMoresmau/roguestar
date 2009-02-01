{-# LANGUAGE Arrows #-}

module VisibleObject
    (ToolThreadInput(..),
     visibleObjects,
     allObjects,
     wieldedParent,
     wieldedTool,
     isBeingWielded,
     visibleObject,
     visibleObjectUniqueID,
     objectDetailsLookup,
     objectDestination,
     objectIdealPosition,
     objectFacing,
     objectIdealFacing,
     objectIdealOrientation,
     visibleObjectHeader,
     wieldableObjectIdealOrientation)
    where

import RSAGL.FRP
import RSAGL.Edge
import Data.Maybe
import ProtocolTypes
import Animation
import Tables
import Control.Arrow
import Data.List
import RSAGL.Time
import RSAGL.InverseKinematics
import RSAGL.Vector
import RSAGL.Angle
import RSAGL.Affine
import RSAGL.CoordinateSystems
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad

data ToolThreadInput = ToolThreadInput {
    tti_wield_points :: Map.Map Integer CoordinateSystem }

-- | Header function that just kills the current thread if it isn't a visible object.
visibleObjectHeader :: RSAnimA (Maybe Integer) i o () ()
visibleObjectHeader = proc () ->
    do unique_id <- arr (fromMaybe (error "visibleObjectHeader: threadIdentity was Nothing")) <<< threadIdentity -< ()
       uids <- allObjects -< ()
       killThreadIf -< isNothing $ find (== unique_id) uids

-- | List all 'VisibleObject' records.
visibleObjects :: RSAnimAX any t i o () [VisibleObject]
visibleObjects = proc () -> arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("visible-objects","0")

-- | List all object UIDs.
allObjects :: RSAnimAX any a i o () [Integer]
allObjects = proc () ->
    do visible_object_ids <- arr (map vo_unique_id . maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("visible-objects","0")
       wielded_object_ids <- arr (map wo_unique_id . maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< Set.toList $ Set.fromList visible_object_ids `Set.union` Set.fromList wielded_object_ids

-- | As a Tool, who is wielding you?
wieldedParent :: RSAnimA (Maybe Integer) i o () (Maybe Integer)
wieldedParent = proc () -> 
    do unique_id <- visibleObjectUniqueID -< ()
       wielded_pairs <- arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< fmap wo_creature_id $ find ((== unique_id) . wo_unique_id) wielded_pairs

-- | As a creature, what tool are you wielding?
wieldedTool :: RSAnimA (Maybe Integer) i o () (Maybe Integer)
wieldedTool = proc () ->
    do unique_id <- visibleObjectUniqueID -< ()
       wielded_pairs <- arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< fmap wo_unique_id $ find ((== unique_id) . wo_creature_id) wielded_pairs

-- | As a Tool, are you currently being wielded?
isBeingWielded :: RSAnimA (Maybe Integer) i o () Bool
isBeingWielded = wieldedParent >>> arr isJust

visibleObjectUniqueID :: RSAnimA (Maybe Integer) i o () Integer
visibleObjectUniqueID = arr (fromMaybe (error "visibleObjectUniqueID: threadIdentity was Nothing")) <<< threadIdentity

-- | Get the 'VisibleObject' record for the current object.
visibleObject :: RSAnimA (Maybe Integer) i o () (Maybe VisibleObject)
visibleObject = proc () ->
    do unique_id <- visibleObjectUniqueID -< ()
       visible_objects <- visibleObjects -< ()
       returnA -< do find ((== unique_id) . vo_unique_id) visible_objects

-- | Get an "object-details" field for the current visible object.
objectDetailsLookup :: String -> RSAnimA (Maybe Integer) i o () (Maybe String)
objectDetailsLookup field = proc _ ->
    do unique_id <- visibleObjectUniqueID -< ()
       m_details_table <- driverGetTableA -< ("object-details",show unique_id)
       sticky isJust Nothing -< (\x -> tableLookup x ("property","value") field) =<< m_details_table

-- | Grid position to which the current object should move.
objectDestination :: RSAnimA (Maybe Integer) i o () (Maybe (Integer,Integer))
objectDestination = arr (fmap vo_xy) <<< visibleObject

-- | Correct position of the current object, with smooth translation.
objectIdealPosition :: RSAnimA (Maybe Integer) i o () (Maybe Point3D)
objectIdealPosition = 
    whenJust (approachA 0.25 (perSecond 3)) <<< 
    arr (fmap (\(x,y) -> Point3D (realToFrac x) 0 (negate $ realToFrac y))) <<< 
    objectDestination

-- | Goal direction in which the current object should be pointed.
objectFacing :: RSAnimA (Maybe Integer) i o () (Maybe BoundAngle)
objectFacing = arr (fmap vo_facing) <<< visibleObject

-- | Direction in which the current object should be pointed, with smooth rotation.
objectIdealFacing :: RSAnimA (Maybe Integer) i o () (Maybe Angle)
objectIdealFacing = arr (fmap unboundAngle) <<< whenJust (approachA 0.1 (perSecond 1)) <<< objectFacing

-- | Combine 'objectIdealPosition' and 'objectIdealFacing' to place an object.
objectIdealOrientation :: RSAnimA (Maybe Integer) i o () (Maybe CoordinateSystem)
objectIdealOrientation = proc () ->
    do m_p <- objectIdealPosition -< ()
       m_a <- objectIdealFacing -< ()
       returnA -< do p <- m_p
                     a <- m_a
		     return $ translate (vectorToFrom p origin_point_3d) $ rotateY a $ root_coordinate_system

-- | 'objectIdealOrientation' implementation that is aware of wield points for wieldable objects.  If an object is being
-- wielded, it will snap to it's wield point.
wieldableObjectIdealOrientation :: RSAnimA (Maybe Integer) i o ToolThreadInput (Maybe CoordinateSystem)
wieldableObjectIdealOrientation = proc tti ->
    do ideal_resting <- objectIdealOrientation -< ()
       m_wielded_parent <- wieldedParent -< ()
       let wield_point = do wielded_parent <- m_wielded_parent
                            Map.lookup wielded_parent $ tti_wield_points tti
       returnA -< wield_point `mplus` ideal_resting
