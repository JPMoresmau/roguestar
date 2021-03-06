{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module Tool
    (dbPickupTool,
     dbWieldTool,
     dbDropTool,
     dbAvailablePickups,
     availableWields,
     dbGetWielded,
     deleteTool,
     toolDurability)
    where

import DB
import Control.Monad.Error
import Data.Maybe
import Data.List as List
import ToolData
import Substances

dbPickupTool :: (DBReadable db,LocationParent a) =>
                CreatureRef ->
                Location ToolRef a ->
                db (Location ToolRef Inventory)
dbPickupTool c l =
    do (c_where :: Maybe (Position,PlaneRef)) <- liftM extractParent $ dbWhere c
       when ((c_where /= extractParent l && Just c /= extractParent l) || isNothing c_where) $ 
	         throwError (DBErrorFlag ToolIs_NotAtFeet)
       return $ toInventory (Inventory c) l

-- | Move a tool into wielded position for whatever creature is carrying it.
dbWieldTool :: (DBReadable db,LocationParent a) =>
               Location ToolRef a -> db (Location ToolRef Wielded)
dbWieldTool l =
    case () of
        () | Just l' <- coerceParent l -> return l' -- if it coerces into our return type, then it's already wielded
        () | Just (Dropped plane_ref position) <- extractParent l ->
            do pickupers <- liftM (map child . filter ((== position) . parent)) $ dbGetContents plane_ref
               case pickupers of -- the creature that is standing over the tool -- there can be only one
                   [single_pickuper] -> return $ toWielded (Wielded single_pickuper) l
                   _ -> throwError $ DBError "dbWieldTool: there were multiple creatures in reach of a single tool"
        () | Just (Inventory c) <- extractParent l -> return $ toWielded (Wielded c) l
        () | otherwise -> throwError $ DBErrorFlag ToolIs_NotWieldable

dbDropTool :: (DBReadable db,LocationParent a) =>
              Location ToolRef a -> db (Location ToolRef Dropped)
dbDropTool l =
    do lp <- liftM extractParent $ dbWhere (genericParent l)
       flip (maybe (throwError $ DBErrorFlag NotStanding)) lp $ \(creature_position,plane_ref) ->
           do return $ toDropped (Dropped plane_ref creature_position) l

dbAvailablePickups :: (DBReadable db) => CreatureRef -> db [ToolRef]
dbAvailablePickups creature_ref =
    do m_creature_where <- liftM extractParent $ dbWhere creature_ref
       flip (maybe (return [])) m_creature_where $ \(creature_position :: Position,plane_ref :: PlaneRef) ->
           do contents <- dbGetContents plane_ref
              return $ map child $ filter ((== creature_position) . parent) contents

-- | List of tools that the specified creature may choose to wield.
-- That is, they are either on the ground or in the creature's inventory.
availableWields :: (DBReadable db) => CreatureRef -> db [ToolRef]
availableWields creature_ref = liftM2 List.union (dbAvailablePickups creature_ref) (dbGetContents creature_ref)

dbGetWielded :: (DBReadable db) => CreatureRef -> db (Maybe ToolRef)
dbGetWielded = liftM (listToMaybe . map (child . asLocationTyped _tool _wielded)) . dbGetContents

-- | Safely delete tools.
deleteTool :: ToolRef -> DB ()
deleteTool = dbUnsafeDeleteObject (error "deleteTool: impossible case: tools shouldn't contain anything")

toolDurability :: (DBReadable db) => ToolRef -> db Integer
toolDurability tool_ref = 
    do t <- dbGetTool tool_ref
       return $ case t of
          DeviceTool _ d -> deviceDurability d
          Sphere (MaterialSubstance m) -> material_construction_value (materialValue m) + 10
          Sphere (GasSubstance {}) -> 10
          Sphere (ChromaliteSubstance {}) -> 110
