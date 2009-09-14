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
     VisibleObjectReference(..),
     getVisibleObject,
     isBeingWielded,
     isWielding,
     visibleObject,
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
import Data.Maybe
import ProtocolTypes
import Animation
import Tables
import Control.Arrow
import Data.List
import RSAGL.Animation
import RSAGL.Math
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
questionMarkAvatar :: RSAnimAX Threaded (Maybe Integer) i o i (Maybe CreatureThreadOutput)
questionMarkAvatar = proc _ ->
    do visibleObjectHeader -< ()
       t <- threadTime -< ()
       m_object_type <- objectDetailsLookup ThisObject "object-type" -< ()
       m_species <- objectDetailsLookup ThisObject "species" -< ()
       m_tool <- objectDetailsLookup ThisObject "tool" -< ()                
       debugOnce -< if any (isJust) [m_object_type,m_species,m_tool] 
                    then (Just $ "questionMarkAvatar: apparently didn't recognize object: " ++ 
		                 show m_object_type ++ ", " ++ show m_species ++ ", " ++ show m_tool)
		    else Nothing
       m_position <- objectIdealPosition ThisObject -< ()
       let float_y = sine $ fromRotations $ t `cyclical'` (fromSeconds 5)
       let m_transform = fmap (translate (Vector3D 0 (0.7 + float_y/10) 0)) m_position 
       transformA libraryA -< maybe (Affine id,(scene_layer_local,NullModel)) (\p -> (Affine $ translateToFrom p origin_point_3d,(scene_layer_local,QuestionMark))) m_transform
       m_wield_point <- whenJust exportCoordinateSystem -< fmap (\p -> translate (vectorToFrom p origin_point_3d `add` Vector3D 0.4 0 0)) m_transform 
       returnA -< 
           do wield_point <- m_wield_point
	      return $ CreatureThreadOutput {
                           cto_wield_point = wield_point }


-- | Launch threads to represent every visible object.
visibleObjectThreadLauncher :: (AbstractEmpty o) => RSAnimAX Threaded (Maybe Integer) i o i o -> RSAnimAX Threaded (Maybe Integer) i o i o
visibleObjectThreadLauncher avatarA = arr (const abstractEmpty) <<< spawnThreads <<< arr (map (\x -> (Just x,avatarA))) <<< allObjects <<< arr (const ())

-- | Kill a thread if an object has the wrong \"object-type\" field, e.g. anything that isn't a \"creature\".
objectTypeGuard :: (String -> Bool) -> RSAnimAX Threaded (Maybe Integer) a b () ()
objectTypeGuard f = proc () ->
    do m_obj_type <- objectDetailsLookup ThisObject "object-type" -< ()
       killThreadIf -< maybe False (not . f) m_obj_type

-- | Header function that just kills the current thread if it isn't a visible object.
visibleObjectHeader :: RSAnimAX Threaded (Maybe Integer) i o () ()
visibleObjectHeader = proc () ->
    do unique_id <- arr (fromMaybe (error "visibleObjectHeader: threadIdentity was Nothing")) <<< threadIdentity -< ()
       uids <- allObjects -< ()
       killThreadIf -< isNothing $ find (== unique_id) uids

-- | List all 'VisibleObject' records.
visibleObjects :: RSAnimAX k t i o () [VisibleObject]
visibleObjects = proc () -> arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("visible-objects","0")

-- | List all object UIDs.
allObjects :: RSAnimAX k t i o () [Integer]
allObjects = proc () ->
    do visible_object_ids <- arr (map vo_unique_id . maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("visible-objects","0")
       wielded_object_ids <- arr (map wo_unique_id . maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< Set.toList $ Set.fromList visible_object_ids `Set.union` Set.fromList wielded_object_ids


{---------------------------------------------------------------------------------------------------
  Retrieving information about specific visible objects.
----------------------------------------------------------------------------------------------------}

-- | Indirect or direct reference to a visible object.
data VisibleObjectReference =
    ThisObject
  | UniqueID Integer
  | WieldedParent VisibleObjectReference
  | WieldedTool VisibleObjectReference
  | Attacker

-- | As a Tool, who is wielding you?
wieldedParent :: RSAnimAX k t i o (Maybe Integer) (Maybe Integer)
wieldedParent = proc m_unique_id -> 
    do wielded_pairs <- arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< fmap wo_creature_id $ find ((== m_unique_id) . Just . wo_unique_id) wielded_pairs

-- | As a creature, what tool are you wielding?
wieldedTool :: RSAnimAX k t i o (Maybe Integer) (Maybe Integer)
wieldedTool = proc m_unique_id ->
    do wielded_pairs <- arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< fmap wo_unique_id $ find ((== m_unique_id) . Just . wo_creature_id) wielded_pairs

-- | Get the unique ID of the object specified.
getVisibleObject :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe Integer)
getVisibleObject (ThisObject) = threadIdentity
getVisibleObject (UniqueID unique_id) = proc () -> returnA -< Just unique_id
getVisibleObject (WieldedParent (WieldedTool ref)) = getVisibleObject ref
getVisibleObject (WieldedTool (WieldedParent ref)) = getVisibleObject ref
getVisibleObject (WieldedParent ref) = getVisibleObject ref >>> wieldedParent
getVisibleObject (WieldedTool ref) = getVisibleObject ref >>> wieldedTool
getVisibleObject Attacker = arr (const "who-attacks") >>> driverGetAnswerA >>> arr (maybe Nothing readInteger)

-- | As a Tool, are you currently being wielded by anyone?
isBeingWielded :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () Bool
isBeingWielded obj = getVisibleObject (WieldedParent obj) >>> arr isJust

-- | As a Creature, are you currently wielding a tool?
isWielding :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () Bool
isWielding obj = getVisibleObject (WieldedTool obj) >>> arr isJust

-- | Get the 'VisibleObject' record for any object.
visibleObject :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe VisibleObject)
visibleObject obj = proc () ->
    do m_unique_id <- getVisibleObject obj -< ()
       visible_objects <- visibleObjects -< ()
       returnA -< do find ((== m_unique_id) . Just . vo_unique_id) visible_objects

-- | Get an "object-details" field for the specified visible object.
objectDetailsLookup :: VisibleObjectReference -> String -> RSAnimAX k (Maybe Integer) i o () (Maybe String)
objectDetailsLookup obj field = proc _ ->
    do m_unique_id <- getVisibleObject obj -< ()
       m_details_table <- arr (fromMaybe Nothing) <<< whenJust (driverGetTableA <<< arr (\x -> ("object-details",show x))) -< m_unique_id
       sticky isJust Nothing -< 
           do details_table <- m_details_table
              tableLookup details_table ("property","value") field

-- | Grid position to which the specified object should move.
objectDestination :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe (Integer,Integer))
objectDestination obj = arr (fmap vo_xy) <<< visibleObject obj

-- | Correct position of the specified object, with smooth translation.
objectIdealPosition :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe Point3D)
objectIdealPosition obj = 
    whenJust (approachA 0.25 (perSecond 3)) <<< 
    arr (fmap (\(x,y) -> Point3D (realToFrac x) 0 (negate $ realToFrac y))) <<< 
    objectDestination obj

-- | Goal direction in which the specified object should be pointed.
objectFacing :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe BoundAngle)
objectFacing obj = arr (fmap vo_facing) <<< visibleObject obj

-- | Direction in which the specified object should be pointed, with smooth rotation.
objectIdealFacing :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe Angle)
objectIdealFacing obj = arr (fmap unboundAngle) <<< whenJust (approachA 0.1 (perSecond 1)) <<< objectFacing obj

-- | Combine 'objectIdealPosition' and 'objectIdealFacing' to place an object.
objectIdealOrientation :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe CoordinateSystem)
objectIdealOrientation obj = proc () ->
    do m_p <- objectIdealPosition obj -< ()
       m_a <- objectIdealFacing obj -< ()
       returnA -< do p <- m_p
                     a <- m_a
		     return $ translate (vectorToFrom p origin_point_3d) $ rotateY a $ root_coordinate_system

-- | 'objectIdealOrientation' implementation that is aware of wield points for wieldable objects.  If an object is being
-- wielded, it will snap to it's wield point.
wieldableObjectIdealOrientation :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o ToolThreadInput (Maybe CoordinateSystem)
wieldableObjectIdealOrientation obj = proc tti ->
    do ideal_resting <- objectIdealOrientation obj -< ()
       m_wielded_parent <- getVisibleObject (WieldedParent obj) -< ()
       let wield_point = do wielded_parent <- m_wielded_parent
                            Map.lookup wielded_parent $ tti_wield_points tti
       returnA -< wield_point `mplus` ideal_resting
