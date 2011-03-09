{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies #-}

module AnimationCreatures
    (creatureAvatar)
    where

import RSAGL.FRP
import RSAGL.Math
import RSAGL.Animation
import RSAGL.Color.RSAGLColors
import Animation
import Control.Arrow
import Models.LibraryData
import VisibleObject
import Limbs
import Scene
import AnimationExtras
import AnimationVortex
import CreatureData

-- | Avatar for any creature that automatically switches to the appropriate species-specific avatar thread.
creatureAvatar :: (FRPModel m) => CreatureAvatar e m
creatureAvatar = proc () ->
    do objectTypeGuard (== "creature") -< ()
       m_species <- objectDetailsLookup ThisObject "species" -< ()
       switchContinue -< (fmap switchTo m_species,())
       returnA -< Nothing
  where switchTo "encephalon" = encephalonAvatar
        switchTo "recreant" = recreantAvatar
        switchTo "androsynth" = androsynthAvatar
        switchTo "ascendant" = ascendantAvatar
        switchTo "caduceator" = caduceatorAvatar
        switchTo "reptilian" = reptilianAvatar
        switchTo "hellion" = hellionAvatar
        switchTo "dustvortex" = dustVortexAvatar
        switchTo _ = questionMarkAvatar

encephalonAvatar :: (FRPModel m) => CreatureAvatar e m
encephalonAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (scene_layer_local,Encephalon)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms MachineArmUpper MachineArmLower (Vector3D 0.66 0.66 0) (Point3D 0.145 0.145 0) 0.33 (Point3D 0.35 0.066 0.133) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

recreantAvatar :: (FRPModel m) => CreatureAvatar e m
recreantAvatar = genericCreatureAvatar $ floatBobbing 0.25 0.4 $ proc () ->
    do libraryA -< (scene_layer_local,Recreant)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms MachineArmUpper MachineArmLower (Vector3D 0 (-1.0) 0) (Point3D 0.3 0.075 0) 0.5 (Point3D 0.5 0.075 0.2) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

androsynthAvatar :: (FRPModel m) => CreatureAvatar e m
androsynthAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (scene_layer_local,Androsynth)
       bothLegs ThinLimb ThinLimb Upright (Vector3D 0 0 1) (Point3D (0.07) 0.5 (-0.08)) 0.55 (Point3D 0.07 0 0.0) -< ()
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms ThinLimb ThinLimb (Vector3D (1.0) (-1.0) (-1.0)) (Point3D 0.05 0.65 0.0) 0.45 (Point3D 0.15 0.34 0.1) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

ascendantAvatar :: (FRPModel m) => CreatureAvatar e m
ascendantAvatar = particleAvatar vortex 12 (SimpleModel AscendantGlow) $ Just light_blue

dust_vortex :: Vortex
dust_vortex = vortex {
    vortex_rotation = \x -> if x > 0.001 then recip x else 0,
    vortex_binding = 0,
    vortex_containment = 0.0,
    vortex_base_angle = fromDegrees 45,
    vortex_repulsion = 0.4,
    vortex_height = -0.1,
    vortex_gravity = 15,
    vortex_base_force = 120 }

dustVortexAvatar :: (FRPModel m) => CreatureAvatar e m
dustVortexAvatar = particleAvatar dust_vortex 12 (SimpleModel DustPuff) Nothing

caduceatorAvatar :: (FRPModel m) => CreatureAvatar e m
caduceatorAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (scene_layer_local,Caduceator)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms CaduceatorArmUpper CaduceatorArmLower (Vector3D 1.0 (-1.0) 1.0) (Point3D 0.1 0.15 0.257) 0.34 (Point3D 0.02 0.17 0.4) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

reptilianAvatar :: (FRPModel m) => CreatureAvatar e m
reptilianAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (scene_layer_local,Reptilian)
       bothLegs ReptilianLegUpper ReptilianLegLower Upright (Vector3D 0 0 1) (Point3D (0.05) 0.25 (-0.1)) 0.29 (Point3D 0.07 0 0.0) -< ()
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms ReptilianArmUpper ReptilianArmLower (Vector3D 1.0 0.0 1.0) (Point3D (0.05) 0.35 (-0.1)) 0.25 (Point3D 0.07 0.25 0.12) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

hellionAvatar :: (FRPModel m) => CreatureAvatar e m
hellionAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (scene_layer_local,Hellion)
       bothEyeStalks (SimpleModel HellionAppendage)
                     (SimpleModel HellionAppendage)
                     (SimpleModel HellionEye)
                     (Vector3D (0.1) 0 (-1))
                     (Point3D 0.06 0.55 0)
                     1.2
                     (Point3D 0.2 0.8 0.05) -< ()
       bothLegs HellionAppendage HellionAppendage Upright (Vector3D 0.5 0 (-1)) (Point3D 0.05 0.55 0) 0.8 (Point3D 0.05 0 0) -< ()
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms HellionAppendage HellionAppendage (Vector3D 1.0 0.0 (-0.5)) (Point3D 0.1 0.6 0) 0.4 (Point3D 0.3 0.25 0.3) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }


