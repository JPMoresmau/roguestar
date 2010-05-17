{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies, FlexibleContexts #-}

module AnimationBuildings
    (buildingAvatar)
    where

import RSAGL.FRP
import Animation
import VisibleObject
import Models.LibraryData
import Control.Arrow
import Scene

type BuildingAvatarSwitch m = AvatarSwitch () () m
type BuildingAvatar e m = FRP e (BuildingAvatarSwitch m) () ()

buildingAvatar :: (FRPModel m) => BuildingAvatar e m
buildingAvatar = proc () ->
    do objectTypeGuard (== "building") -< ()
       m_building_type <- objectDetailsLookup ThisObject "building-type" -< ()
       switchContinue -< (fmap switchTo m_building_type,())
       returnA -< ()
  where switchTo "monolith" = simpleBuildingAvatar Monolith
        switchTo "portal" = simpleBuildingAvatar Portal
        switchTo _ = questionMarkAvatar >>> arr (const ())

simpleBuildingAvatar :: (FRPModel m, LibraryModelSource lm) =>
                        lm -> BuildingAvatar e m
simpleBuildingAvatar phase_weapon_model = proc () ->
    do visibleObjectHeader -< ()
       m_orientation <- objectIdealOrientation ThisObject -< ()
       whenJust (transformA libraryA) -< fmap
           (\o -> (o,(scene_layer_local,phase_weapon_model))) m_orientation
       returnA -< ()


