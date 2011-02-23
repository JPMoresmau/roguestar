{-# LANGUAGE Arrows #-}

module CreatureData
    (CreatureAvatarSwitch,
     CreatureAvatar,
     genericCreatureAvatar)
    where

import RSAGL.FRP
import RSAGL.Scene
import VisibleObject
import Data.Maybe
import Control.Arrow

type CreatureAvatarSwitch m = AvatarSwitch () (Maybe CreatureThreadOutput) m
type CreatureAvatar e m = FRP e (AvatarSwitch () (Maybe CreatureThreadOutput) m) () (Maybe CreatureThreadOutput)

genericCreatureAvatar :: (FRPModel m) => FRP e (CreatureAvatarSwitch m) () CreatureThreadOutput -> CreatureAvatar e m
genericCreatureAvatar creatureA = proc () ->
    do visibleObjectHeader -< ()
       m_orientation <- objectIdealOrientation ThisObject -< ()
       switchTerminate -< if isNothing m_orientation then (Just $ genericCreatureAvatar creatureA,Nothing) else (Nothing,Nothing)
       arr Just <<< transformA creatureA -< (fromMaybe (error "genericCreatureAvatar: fromMaybe") m_orientation,())

