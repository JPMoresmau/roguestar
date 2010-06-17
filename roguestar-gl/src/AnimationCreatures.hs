{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies #-}

module AnimationCreatures
    (creatureAvatar)
    where

import RSAGL.FRP
import RSAGL.Math
import RSAGL.Animation
import RSAGL.Modeling.RSAGLColors
import Animation
import Control.Arrow
import Data.Maybe
import Models.LibraryData
import VisibleObject
import Limbs
import Scene
import AnimationExtras

type CreatureAvatarSwitch m = AvatarSwitch () (Maybe CreatureThreadOutput) m
type CreatureAvatar e m = FRP e (AvatarSwitch () (Maybe CreatureThreadOutput) m) () (Maybe CreatureThreadOutput)

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
        switchTo _ = questionMarkAvatar

genericCreatureAvatar :: (FRPModel m) => FRP e (CreatureAvatarSwitch m) () CreatureThreadOutput -> CreatureAvatar e m
genericCreatureAvatar creatureA = proc () ->
    do visibleObjectHeader -< ()
       m_orientation <- objectIdealOrientation ThisObject -< ()
       switchTerminate -< if isNothing m_orientation then (Just $ genericCreatureAvatar creatureA,Nothing) else (Nothing,Nothing)
       arr Just <<< transformA creatureA -< (fromMaybe (error "genericCreatureAvatar: fromMaybe") m_orientation,())

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
       bothLegs ThinLimb ThinLimb (Vector3D 0 0 1) (Point3D (0.07) 0.5 (-0.08)) 0.7 (Point3D 0.07 0 0.0) -< ()
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms ThinLimb ThinLimb (Vector3D (1.0) (-1.0) (-1.0)) (Point3D 0.05 0.65 0.0) 0.45 (Point3D 0.15 0.34 0.1) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

glower :: (FRPModel m, StateOf m ~ AnimationState, ThreadIDOf m ~ Maybe Integer) => Point3D -> Vector3D -> FRP e m () ()
glower p_init v_init = proc () ->
    do local_origin <- exportToA root_coordinate_system -< origin_point_3d
       transformA
           (accelerationModel fps120 (p_init,perSecond $ v_init) 
                 (proc () -> 
	             do a <- derivative <<< derivative <<< exportToA root_coordinate_system -< origin_point_3d
	 	        returnA -< concatForces [quadraticTrap 10 p_init,
	                                         drag 1.0,
			    		         \_ _ _ -> scalarMultiply (-1) a,
						 \_ _ _ -> perSecond $ perSecond v_init,
						 \_ p _ -> perSecond $ perSecond $ vectorNormalize $
						               vectorToFrom origin_point_3d p `crossProduct` v_init]) 
	         (proc (_,()) -> libraryPointAtCamera -< (scene_layer_local,AscendantGlow))) -< 
	             (translateToFrom local_origin origin_point_3d $ root_coordinate_system,())

ascendantAvatar :: (FRPModel m) => CreatureAvatar e m
ascendantAvatar = genericCreatureAvatar $ proc () ->
    do glower (Point3D 0 0.5 0) zero -< ()
       glower (Point3D 0 0.5 0.35) (Vector3D 0 0 (-1)) -< ()
       glower (Point3D 0 0.5 (-0.35)) (Vector3D 0 0 1) -< ()
       glower (Point3D 0.35 0.5 0) (Vector3D (-1) 0 0) -< ()
       glower (Point3D (-0.35) 0.5 0) (Vector3D 1 0 0) -< ()
       accumulateSceneA -< (scene_layer_local,
                            lightSource $ PointLight (Point3D 0 0.5 0)
                                                     (measure (Point3D 0 0.5 0) (Point3D 0 0 0))
						     azure
						     azure)
       t <- threadTime -< ()
       wield_point <- exportCoordinateSystem -< translate (rotateY (fromRotations $ t `cyclical'` (fromSeconds 3)) $ Vector3D 0.25 0.5 0)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

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
       bothLegs ReptilianLegUpper ReptilianLegLower (Vector3D 0 0 1) (Point3D (0.05) 0.25 (-0.1)) 0.29 (Point3D 0.07 0 0.0) -< ()
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms ReptilianArmUpper ReptilianArmLower (Vector3D 1.0 0.0 1.0) (Point3D (0.05) 0.35 (-0.1)) 0.25 (Point3D 0.07 0.25 0.12) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

