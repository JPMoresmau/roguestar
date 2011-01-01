{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies, FlexibleContexts, RankNTypes #-}

module AnimationBuildings
    (buildingAvatar)
    where

import RSAGL.FRP
import RSAGL.Math
import RSAGL.Animation
import RSAGL.Color.RSAGLColors
import Animation
import VisibleObject
import Models.LibraryData
import Control.Arrow
import Scene

type BuildingAvatarSwitch m = AvatarSwitch () () m
type BuildingAvatar e m = FRP e (BuildingAvatarSwitch m) () ()

-- | An avatar for a building.  This function
-- detects the type of a building based on the
-- FRP Thread ID, and switches to the appropriate
-- type of building avatar.
buildingAvatar :: (FRPModel m) => BuildingAvatar e m
buildingAvatar = proc () ->
    do objectTypeGuard (== "building") -< ()
       m_building_type <- objectDetailsLookup ThisObject "building-type" -< ()
       switchContinue -< (fmap switchTo m_building_type,())
       returnA -< ()
  where switchTo "monolith" = simpleBuildingAvatar Monolith
        switchTo "anchor" = planetaryAnchorAvatar
        switchTo "portal" = simpleBuildingAvatar Portal
        switchTo _ = questionMarkAvatar >>> arr (const ())

simpleBuildingAvatar :: (FRPModel m, LibraryModelSource lm) =>
                        lm -> BuildingAvatar e m
simpleBuildingAvatar building_model = genericBuildingAvatar $ proc () ->
    do libraryA -< (scene_layer_local,building_model)
       returnA -< ()

genericBuildingAvatar :: (FRPModel m) =>
                         (forall x y. FRP e (FRP1Context x y (BuildingAvatarSwitch m)) () ()) ->
                         BuildingAvatar e m
genericBuildingAvatar actionA = proc () ->
    do visibleObjectHeader -< ()
       m_orientation <- objectIdealOrientation ThisObject -< ()
       whenJust (transformA actionA) -< fmap
           (\o -> (o,())) m_orientation
       returnA -< ()

planetaryAnchorAvatar :: (FRPModel m) => BuildingAvatar e m
planetaryAnchorAvatar = genericBuildingAvatar $ translate (Vector3D 0 1.0 0) $ proc () ->
    do libraryA -< (scene_layer_local,PlanetaryAnchorCore)
       planetaryAnchorFlange (1.1^1) (fromDegrees 25) (fromDegrees 30) 10.0 -< ()
       planetaryAnchorFlange (1.1^2) (fromDegrees 50) (fromDegrees 60) 9.0 -< ()
       planetaryAnchorFlange (1.1^3) (fromDegrees 75) (fromDegrees 90) 7.0 -< ()
       planetaryAnchorFlange (1.1^4) (fromDegrees 100) (fromDegrees 120) 4.0 -< ()
       planetaryAnchorFlange (1.1^5) (fromDegrees 125) (fromDegrees 150) 1.0 -< ()
       accumulateSceneA -< (scene_layer_local,
                            lightSource $ PointLight (Point3D 0 1.0 0)
                                                     (measure (Point3D 0 1.0 0) (Point3D 1 0 1))
                                                     white
                                                     violet)

planetaryAnchorFlange :: (FRPModel m, StateOf m ~ AnimationState, InputOutputOf m ~ Enabled) =>
                         RSdouble -> Angle -> Angle -> RSdouble -> FRP e m () ()
planetaryAnchorFlange s rx rz x = scale' s $ proc () ->
    do rotateA (Vector3D 0 1 0) (perSecond $ fromDegrees $ x*3.0) (rotate (Vector3D 0 0 1) rz $
           rotateA (Vector3D 0 0 1) (perSecond $ fromDegrees $ x*7.0) (rotate (Vector3D 1 0 0) rx $
               rotateA (Vector3D 1 0 0) (perSecond $ fromDegrees $ x*2.0) libraryA)) -<
                   (scene_layer_local,PlanetaryAnchorFlange)


