{-# LANGUAGE PatternSignatures #-}

module Tool
    (dbPickupTool,
     dbWieldTool,
     dbDropTool,
     dbAvailablePickups,
     dbGetInventory,
     dbGetCarried,
     dbGetWielded)
    where

import DB
import Control.Monad.Error
import Data.Maybe

dbPickupTool :: (DBReadable db) => CreatureRef -> Location s ToolRef a -> db (Location s ToolRef Inventory)
dbPickupTool c l = 
    do (c_where :: Maybe (Position,PlaneRef)) 
           <- liftM extractLocation $ dbWhere c
       when ((c_where /= extractLocation l && Just c /= extractLocation l) || isNothing c_where) $ 
	         throwError (DBErrorFlag "not-at-feet")
       return $ pickupTool (Inventory c) l

dbWieldTool :: (DBReadable db) => Location s ToolRef a -> db (Location s ToolRef Wielded)
dbWieldTool l =
    case extractLocation l of
        _ | isWieldedLocation l -> throwError (DBErrorFlag "already-wielded")
        Just (Inventory c) -> return $ wieldTool (Wielded c) l
        Nothing -> throwError (DBErrorFlag "not-in-inventory")

dbDropTool :: (DBReadable db) => Location s ToolRef a -> db (Location s ToolRef Dropped)
dbDropTool l =
    do lp <- liftM extractLocation $ dbWhere (genericParent l)
       flip (maybe (throwError $ DBErrorFlag "not-standing")) lp $ \(creature_position,plane_ref) ->
           do return $ dropTool (Dropped plane_ref creature_position) l

dbAvailablePickups :: (DBReadable db) => CreatureRef -> db [ToolRef]
dbAvailablePickups creature_ref =
    do m_creature_where <- liftM extractLocation $ dbWhere creature_ref
       flip (maybe (return [])) m_creature_where $ \(creature_position :: Position,plane_ref :: PlaneRef) ->
           do contents <- liftM (mapMaybe coerceLocation) $ dbGetContents plane_ref
              return $ map entity $ filter ((== creature_position) . location) contents

dbGetInventory :: (DBReadable db) => CreatureRef -> db [ToolRef]
dbGetInventory = liftM (map entity . mapMaybe toToolLocation . mapMaybe toInventoryLocation) . dbGetContents

dbGetCarried :: (DBReadable db) => CreatureRef -> db [ToolRef]
dbGetCarried = liftM (map entity . mapMaybe toToolLocation) . dbGetContents

dbGetWielded :: (DBReadable db) => CreatureRef -> db (Maybe ToolRef)
dbGetWielded = liftM (listToMaybe . map entity . mapMaybe toToolLocation . mapMaybe toWieldedLocation) . dbGetContents
