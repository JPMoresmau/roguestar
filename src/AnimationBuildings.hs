{-# LANGUAGE Arrows #-}

module AnimationBuildings
    (buildingAvatar)
    where

import RSAGL.FRP
import Animation
import VisibleObject
import Models.LibraryData
import Control.Arrow
import Scene

buildingAvatar :: RSAnimAX Threaded (Maybe Integer) () () () ()
buildingAvatar = proc () ->
    do objectTypeGuard (== "building") -< ()
       m_building_type <- objectDetailsLookup ThisObject "building-type" -< ()
       switchContinue -< (fmap switchTo m_building_type,())
       returnA -< ()
  where switchTo "monolith" = simpleBuildingAvatar Monolith
        switchTo "stargate" = simpleBuildingAvatar TreatyStargate
        switchTo _ = questionMarkAvatar >>> arr (const ())

simpleBuildingAvatar :: LibraryModel -> RSAnimAX Threaded (Maybe Integer) () () () ()
simpleBuildingAvatar phase_weapon_model = proc () ->
    do visibleObjectHeader -< ()
       m_orientation <- objectIdealOrientation ThisObject -< ()
       whenJust (transformA libraryA) -< fmap (\o -> (o,(scene_layer_local,phase_weapon_model))) m_orientation
       returnA -< ()


