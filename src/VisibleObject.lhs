\section{Visible Objects}

\begin{code}
{-# LANGUAGE Arrows #-}

module VisibleObject
    (visibleObjects,
     visibleObject,
     objectDetailsLookup,
     objectDestination,
     objectIdealPosition,
     objectFacing,
     objectIdealFacing,
     objectIdealOrientation,
     visibleObjectHeader)
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
import Control.Applicative

visibleObjectHeader :: RSAnimA (Maybe Integer) () () () ()
visibleObjectHeader = proc () ->
    do unique_id <- arr (fromMaybe (error "visibleObjectHeader: threadIdentity was Nothing")) <<< threadIdentity -< ()
       visible_objects <- visibleObjects -< ()
       killThreadIf -< (isNothing $ find ((== unique_id) . vo_unique_id) visible_objects,()) 

visibleObjects :: RSAnimAX any t i o () [VisibleObject]
visibleObjects = proc () -> arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA -< ("visible-objects","0")

visibleObjectUniqueID :: RSAnimAX any (Maybe Integer) i o () Integer
visibleObjectUniqueID = arr (fromMaybe (error "visibleObjectUniqueID: threadIdentity was Nothing")) <<< threadIdentity

visibleObject :: RSAnimAX any (Maybe Integer) i o () (Maybe VisibleObject)
visibleObject = proc () ->
    do unique_id <- visibleObjectUniqueID -< ()
       visible_objects <- visibleObjects -< ()
       returnA -< do find ((== unique_id) . vo_unique_id) visible_objects

objectDetailsLookup :: String -> RSAnimAX any (Maybe Integer) i o () (Maybe String)
objectDetailsLookup field = proc _ ->
    do unique_id <- visibleObjectUniqueID -< ()
       m_details_table <- driverGetTableA -< ("object-details",show unique_id)
       sticky isJust Nothing -< (\x -> tableLookup x ("property","value") field) =<< m_details_table

objectDestination :: RSAnimAX any (Maybe Integer) i o () (Maybe (Integer,Integer))
objectDestination = arr (fmap vo_xy) <<< visibleObject
    
objectIdealPosition :: RSAnimAX any (Maybe Integer) i o () (Maybe Vector3D)
objectIdealPosition = 
    whenJust (approachA 0.25 (perSecond 3)) <<< 
    arr (fmap (\(x,y) -> Vector3D (realToFrac x) 0 (realToFrac y))) <<< 
    objectDestination

objectFacing :: RSAnimAX any (Maybe Integer) i o () (Maybe BoundAngle)
objectFacing = arr (fmap vo_facing) <<< visibleObject

objectIdealFacing :: RSAnimAX any (Maybe Integer) i o () (Maybe Angle)
objectIdealFacing = arr (fmap unboundAngle) <<< whenJust (approachA 0.1 (perSecond 1)) <<< objectFacing

objectIdealOrientation :: RSAnimAX any (Maybe Integer) i o () (Maybe AffineTransformation)
objectIdealOrientation = proc () ->
    do v <- objectIdealPosition -< ()
       a <- objectIdealFacing -< ()
       returnA -< (.) <$> (translate <$> v) <*> (rotateY <$> a)
\end{code}
