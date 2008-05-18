\section{Visible Objects}

\begin{code}
{-# LANGUAGE Arrows #-}

module VisibleObject
    (ToolThreadInput(..),
     visibleObjects,
     allObjects,
     wieldedParent,
     wieldedTool,
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

visibleObjectHeader :: RSAnimA (Maybe Integer) i o () ()
visibleObjectHeader = proc () ->
    do unique_id <- arr (fromMaybe (error "visibleObjectHeader: threadIdentity was Nothing")) <<< threadIdentity -< ()
       uids <- allObjects -< ()
       killThreadIf -< isNothing $ find (== unique_id) uids

visibleObjects :: RSAnimAX any t i o () [VisibleObject]
visibleObjects = proc () -> arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("visible-objects","0")

allObjects :: RSAnimAX any a i o () [Integer]
allObjects = proc () ->
    do visible_object_ids <- arr (map vo_unique_id . maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("visible-objects","0")
       wielded_object_ids <- arr (map wo_unique_id . maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< Set.toList $ Set.fromList visible_object_ids `Set.union` Set.fromList wielded_object_ids

wieldedParent :: RSAnimA (Maybe Integer) i o () (Maybe Integer)
wieldedParent = proc () -> 
    do unique_id <- visibleObjectUniqueID -< ()
       wielded_pairs <- arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< fmap wo_creature_id $ find ((== unique_id) . wo_unique_id) wielded_pairs

wieldedTool :: RSAnimA (Maybe Integer) i o () (Maybe Integer)
wieldedTool = proc () ->
    do unique_id <- visibleObjectUniqueID -< ()
       wielded_pairs <- arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("wielded-objects","0")
       returnA -< fmap wo_unique_id $ find ((== unique_id) . wo_creature_id) wielded_pairs

visibleObjectUniqueID :: RSAnimA (Maybe Integer) i o () Integer
visibleObjectUniqueID = arr (fromMaybe (error "visibleObjectUniqueID: threadIdentity was Nothing")) <<< threadIdentity

visibleObject :: RSAnimA (Maybe Integer) i o () (Maybe VisibleObject)
visibleObject = proc () ->
    do unique_id <- visibleObjectUniqueID -< ()
       visible_objects <- visibleObjects -< ()
       returnA -< do find ((== unique_id) . vo_unique_id) visible_objects

objectDetailsLookup :: String -> RSAnimA (Maybe Integer) i o () (Maybe String)
objectDetailsLookup field = proc _ ->
    do unique_id <- visibleObjectUniqueID -< ()
       m_details_table <- driverGetTableA -< ("object-details",show unique_id)
       sticky isJust Nothing -< (\x -> tableLookup x ("property","value") field) =<< m_details_table

objectDestination :: RSAnimA (Maybe Integer) i o () (Maybe (Integer,Integer))
objectDestination = arr (fmap vo_xy) <<< visibleObject
    
objectIdealPosition :: RSAnimA (Maybe Integer) i o () (Maybe Point3D)
objectIdealPosition = 
    whenJust (approachA 0.25 (perSecond 3)) <<< 
    arr (fmap (\(x,y) -> Point3D (realToFrac x) 0 (negate $ realToFrac y))) <<< 
    objectDestination

objectFacing :: RSAnimA (Maybe Integer) i o () (Maybe BoundAngle)
objectFacing = arr (fmap vo_facing) <<< visibleObject

objectIdealFacing :: RSAnimA (Maybe Integer) i o () (Maybe Angle)
objectIdealFacing = arr (fmap unboundAngle) <<< whenJust (approachA 0.1 (perSecond 1)) <<< objectFacing

objectIdealOrientation :: RSAnimA (Maybe Integer) i o () (Maybe CoordinateSystem)
objectIdealOrientation = proc () ->
    do m_p <- objectIdealPosition -< ()
       m_a <- objectIdealFacing -< ()
       returnA -< do p <- m_p
                     a <- m_a
		     return $ translate (vectorToFrom p origin_point_3d) $ rotateY a $ root_coordinate_system

wieldableObjectIdealOrientation :: RSAnimA (Maybe Integer) i o ToolThreadInput (Maybe CoordinateSystem)
wieldableObjectIdealOrientation = proc tti ->
    do ideal_resting <- objectIdealOrientation -< ()
       m_wielded_parent <- wieldedParent -< ()
       let wield_point = do wielded_parent <- m_wielded_parent
                            Map.lookup wielded_parent $ tti_wield_points tti
       returnA -< wield_point `mplus` ideal_resting
\end{code}
