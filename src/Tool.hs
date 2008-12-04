{-# LANGUAGE PatternSignatures, PatternGuards #-}

module Tool
    (dbPickupTool,
     dbWieldTool,
     dbDropTool,
     dbAvailablePickups,
     availableWields,
     dbGetWielded)
    where

import DB
import Control.Monad.Error
import Data.Maybe
import Data.List as List

dbPickupTool :: (DBReadable db,LocationType a) => CreatureRef -> Location s ToolRef a -> db (Location s ToolRef Inventory)
dbPickupTool c l = 
    do (c_where :: Maybe (Position,PlaneRef)) <- liftM extractLocation $ dbWhere c
       when ((c_where /= extractLocation l && Just c /= extractLocation l) || isNothing c_where) $ 
	         throwError (DBErrorFlag "not-at-feet")
       return $ toInventory (Inventory c) l

-- | Move a tool into wielded position for whatever creature is carrying it.
dbWieldTool :: (DBReadable db,LocationType a) => Location s ToolRef a -> db (Location s ToolRef Wielded)
dbWieldTool l =
    case () of
        () | Just l' <- coerceLocation l -> return l'
        () | Just (Dropped plane_ref position) <- extractLocation l ->
            do pickupers <- liftM (map entity . filter ((== position) . location)) $ dbGetContents plane_ref
               case pickupers of
                   [single_pickuper] -> return $ toWielded (Wielded single_pickuper) l
                   _ -> throwError $ DBErrorFlag "tool-is-not-wieldable"
        () | Just (Inventory c) <- extractLocation l -> return $ toWielded (Wielded c) l
        () | otherwise -> throwError $ DBErrorFlag "tool-is-not-wieldable"

dbDropTool :: (DBReadable db,LocationType a) => Location s ToolRef a -> db (Location s ToolRef Dropped)
dbDropTool l =
    do lp <- liftM extractLocation $ dbWhere (getLocation l)
       flip (maybe (throwError $ DBErrorFlag "not-standing")) lp $ \(creature_position,plane_ref) ->
           do return $ toDropped (Dropped plane_ref creature_position) l

dbAvailablePickups :: (DBReadable db) => CreatureRef -> db [ToolRef]
dbAvailablePickups creature_ref =
    do m_creature_where <- liftM extractLocation $ dbWhere creature_ref
       flip (maybe (return [])) m_creature_where $ \(creature_position :: Position,plane_ref :: PlaneRef) ->
           do contents <- dbGetContents plane_ref
              return $ map entity $ filter ((== creature_position) . location) contents

-- | List of tools that the specified creature may choose to wield.
-- That is, they are either on the ground or in the creature's inventory.
availableWields :: (DBReadable db) => CreatureRef -> db [ToolRef]
availableWields creature_ref = liftM2 List.union (dbAvailablePickups creature_ref) (dbGetContents creature_ref)

dbGetWielded :: (DBReadable db) => CreatureRef -> db (Maybe ToolRef)
dbGetWielded = liftM (listToMaybe . map (entity . asLocationTyped _tool _wielded)) . dbGetContents

