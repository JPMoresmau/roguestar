{-# LANGUAGE Arrows #-}

module AnimationTools
    (toolAvatar)
    where

import RSAGL.Math
import Scene
import Animation
import RSAGL.Animation
import RSAGL.FRP
import Control.Arrow
import VisibleObject
import Models.LibraryData
import Control.Applicative
import EventUtils
import Control.Monad

-- | Avatar for any tool that automatically switched to the correct tool-specific thread.
toolAvatar :: RSAnimAX Threaded (Maybe Integer) ToolThreadInput () ToolThreadInput ()
toolAvatar = proc tti ->
    do objectTypeGuard (== "tool") -< ()
       m_tool <- objectDetailsLookup ThisObject "tool" -< ()
       m_tool_type <- objectDetailsLookup ThisObject "tool-type" -< ()
       switchContinue -< (fmap switchTo $ (,) <$> m_tool_type <*> m_tool,tti)
       returnA -< ()
  where switchTo (_,"phase_pistol") = phaseWeaponAvatar PhasePistol
        switchTo (_,"phaser") = phaseWeaponAvatar Phaser
        switchTo (_,"phase_rifle") = phaseWeaponAvatar PhaseRifle
        switchTo (_,"kinetic_fleuret") = energySwordAvatar Yellow 2
        switchTo (_,"kinetic_sabre") = energySwordAvatar Yellow 4
        switchTo (_,"improvised_pistol") = phaseWeaponAvatar PhasePistol
        switchTo (_,"improvised_carbine") = phaseWeaponAvatar Phaser
        switchTo (_,"improvised_rifle") = phaseWeaponAvatar PhaseRifle
        switchTo (_,"improvised_fleuret") = energySwordAvatar Red 2
        switchTo (_,"improvised_sabre") = energySwordAvatar Red 4
        switchTo ("sphere-gas",gas) = gasSphereAvatar gas
        switchTo ("sphere-material",material) = materialSphereAvatar material
        switchTo ("sphere-chromalite",chromalite) = chromaliteSphereAvatar chromalite
        switchTo _ = questionMarkAvatar >>> arr (const ())

simpleToolAvatar :: LibraryModel -> RSAnimAX Threaded (Maybe Integer) ToolThreadInput () ToolThreadInput ()
simpleToolAvatar phase_weapon_model = proc tti ->
    do visibleObjectHeader -< ()
       m_orientation <- wieldableObjectIdealOrientation ThisObject -< tti
       whenJust (transformA libraryA) -< fmap (\o -> (o,(scene_layer_local,phase_weapon_model))) m_orientation
       returnA -< ()

phaseWeaponAvatar :: LibraryModel -> RSAnimAX Threaded (Maybe Integer) ToolThreadInput () ToolThreadInput ()
phaseWeaponAvatar = simpleToolAvatar

energySwordAvatar :: EnergyColor -> Integer -> RSAnimAX Threaded (Maybe Integer) ToolThreadInput () ToolThreadInput ()
energySwordAvatar energy_color sword_size = proc tti ->
    do visibleObjectHeader -< ()
       m_orientation <- wieldableObjectIdealOrientation ThisObject -< tti
       m_atk_time <- recentAttack (WieldedParent ThisObject) -< ()
       t_now <- threadTime -< ()
       is_being_wielded <- isBeingWielded ThisObject -< ()
       whenJust (transformA $ transformA displayA) -<
           do orientation <- m_orientation
              atk_time <- m_atk_time
              let atk_rotate = case toSeconds (t_now `sub` atk_time) of
                      t | t < 1 -> fromRotations $ t*1.25 -- 1 and 1-quarter revolution in one second
                      t | t >= 1 && t <= 1.5 -> fromRotations $ 1.25 - 0.25 * (t - 1) -- then swing back
                      _ | otherwise -> fromRotations 0
              return $ (orientation,(Affine $ rotate (Vector3D 1 0 0) atk_rotate, is_being_wielded))
         `mplus`
           do orientation <- m_orientation
              return (orientation,(Affine id,is_being_wielded))
       returnA -< ()
  where displayA :: RSAnimAX () () i o Bool ()
        displayA = scale' (1/75) $ proc is_being_wielded ->
            do blade_length <- approachFrom 1 (perSecond 65) 0 -< if is_being_wielded then 10 * realToFrac sword_size else 0
               libraryA -< (scene_layer_local,EnergySword energy_color sword_size)
               transformA libraryA -< (Affine $ translate (Vector3D 0 2.9 0) . scale (Vector3D 1 blade_length 1),(scene_layer_local,EnergyCylinder energy_color))

gasSphereAvatar :: String -> RSAnimAX Threaded (Maybe Integer) ToolThreadInput () ToolThreadInput ()
gasSphereAvatar = simpleToolAvatar . gasToModel
    where gasToModel :: String -> LibraryModel
          gasToModel = const GasSphere

materialSphereAvatar :: String -> RSAnimAX Threaded (Maybe Integer) ToolThreadInput () ToolThreadInput ()
materialSphereAvatar = simpleToolAvatar . materialToModel
    where materialToModel :: String -> LibraryModel
          materialToModel = const MetalSphere

chromaliteSphereAvatar :: String -> RSAnimAX Threaded (Maybe Integer) ToolThreadInput () ToolThreadInput ()
chromaliteSphereAvatar = simpleToolAvatar . const ChromaliteSphere
          
