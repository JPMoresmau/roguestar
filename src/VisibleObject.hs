{-# LANGUAGE Arrows #-}

module VisibleObject
    (ToolThreadInput(..),
     CreatureThreadOutput(..),
     AbstractEmpty(..),
     visibleObjectThreadLauncher,
     objectTypeGuard,
     questionMarkAvatar,
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
import RSAGL.AbstractVector
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Scene
import Models.LibraryData

data ToolThreadInput = ToolThreadInput {
    -- | Wield points of all creatures that are on screen.
    tti_wield_points :: Map.Map Integer CoordinateSystem }

data CreatureThreadOutput = CreatureThreadOutput {
    -- | Wield point for this creature.  This is the coordinate system in which a tool should be rendered, 
    -- where the origin is the point at which the creature is grasping the weapon.
    cto_wield_point :: CoordinateSystem }

class AbstractEmpty a where
    abstractEmpty :: a

instance AbstractEmpty () where
    abstractEmpty = ()

instance AbstractEmpty (Maybe a) where
    abstractEmpty = Nothing

-- | Avatar for unrecognized objects.  Basically this is a big conspicuous warning that
-- something is implemented in the engine but not in the client.
questionMarkAvatar :: RSAnimA (Maybe Integer) i o i (Maybe CreatureThreadOutput)
questionMarkAvatar = proc _ ->
    do visibleObjectHeader -< ()
       t <- threadTime -< ()
       m_object_type <- objectDetailsLookup "object-type" -< ()
       m_species <- objectDetailsLookup "species" -< ()
       m_tool <- objectDetailsLookup "tool" -< ()                
       debugOnce -< if any (isJust) [m_object_type,m_species,m_tool] 
                    then (Just $ "questionMarkAvatar: apparently didn't recognize object: " ++ 
		                 show m_object_type ++ ", " ++ show m_species ++ ", " ++ show m_tool)
		    else Nothing
       m_position <- objectIdealPosition -< ()
       let float_y = sine $ fromRotations $ t `cyclical'` (fromSeconds 5)
       let m_transform = fmap (translate (Vector3D 0 (0.7 + float_y/10) 0)) m_position 
       transformA libraryA -< maybe (Affine id,(scene_layer_local,NullModel)) (\p -> (Affine $ translateToFrom p origin_point_3d,(scene_layer_local,QuestionMark))) m_transform
       m_wield_point <- whenJust exportCoordinateSystem -< fmap (\p -> translate (vectorToFrom p origin_point_3d `add` Vector3D 0.4 0 0)) m_transform 
       returnA -< 
           do wield_point <- m_wield_point
	      return $ CreatureThreadOutput {
                           cto_wield_point = wield_point }


-- | Launch threads to represent every visible object.
visibleObjectThreadLauncher :: (AbstractEmpty o) => RSAnimA (Maybe Integer) i o i o -> RSAnimA (Maybe Integer) i o i o
visibleObjectThreadLauncher avatarA = arr (const abstractEmpty) <<< spawnThreads <<< arr (map (\x -> (Just x,avatarA))) <<< allObjects <<< arr (const ())

-- | Kill a thread if an object has the wrong \"object-type\" field, e.g. anything that isn't a \"creature\".
objectTypeGuard :: (String -> Bool) -> RSAnimA (Maybe Integer) a b () ()
objectTypeGuard f = proc () ->
    do m_obj_type <- objectDetailsLookup "object-type" -< ()
       killThreadIf -< maybe False (not . f) m_obj_type

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
