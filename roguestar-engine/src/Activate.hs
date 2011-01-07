module Activate
    (ActivationOutcome,
     resolveActivation,
     executeActivation)
    where

import Tool
import ToolData
import Creature
import DB
import Control.Monad.Error
import Substances

-- | Outcome of activating a tool.
data ActivationOutcome =
    Heal CreatureRef Integer
  | ExpendTool ToolRef ActivationOutcome
  | NoEffect

resolveActivation :: (DBReadable db) => CreatureRef -> db ActivationOutcome
resolveActivation creature_ref =
    do tool_ref <- maybe (throwError $ DBErrorFlag NoToolWielded) return =<< dbGetWielded creature_ref
       tool <- dbGetTool tool_ref
       case tool of
           DeviceTool {} -> throwError $ DBErrorFlag ToolIs_Innapropriate
           Sphere (ChromaliteSubstance c) ->
               do x <- linearRoll $ chromalitePotency c
                  return $ if x == 0 then ExpendTool tool_ref $ NoEffect
                                     else Heal creature_ref x
           Sphere (MaterialSubstance m) ->
               do x <- linearRoll $ material_construction_value $ materialValue m
                  return $ ExpendTool tool_ref $ Heal creature_ref x
           Sphere (GasSubstance g) ->
               do x <- linearRoll $ gasValue g
                  return $ if x == 0 then ExpendTool tool_ref $ Heal creature_ref 1
                                     else Heal creature_ref 1

executeActivation :: ActivationOutcome -> DB ()
executeActivation (NoEffect) = return ()
executeActivation (Heal creature_ref x) =
    do healCreature x creature_ref
       dbPushSnapshot $ HealEvent creature_ref
executeActivation (ExpendTool tool_ref activation_outcome) =
    do executeActivation activation_outcome
       dbPushSnapshot $ ExpendToolEvent tool_ref
       deleteTool tool_ref

