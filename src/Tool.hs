{-# LANGUAGE PatternSignatures #-}

module Tool
    (dbPickupTool,
     dbAvailablePickups)
    where

import DB
import Control.Monad.Error
import Data.Maybe

dbTeleportToolIntoInventory :: CreatureRef -> ToolRef -> DB ()
dbTeleportToolIntoInventory c t = dbMove t (return . pickupTool (Inventory c)) >> return ()

dbPickupTool :: CreatureRef -> ToolRef -> DB ()
dbPickupTool c t = 
    do c_where <- liftM (fmap location . coerceParent) $ dbWhere c :: DB (Maybe (PlaneRef,Position))
       t_where <- liftM (fmap location . coerceParent) $ dbWhere t :: DB (Maybe (PlaneRef,Position))
       when (c_where /= t_where && isJust c_where) $ throwError (DBErrorFlag "its-not-there")
       dbTeleportToolIntoInventory c t

dbAvailablePickups :: (DBReadable db) => CreatureRef -> db [ToolRef]
dbAvailablePickups creature_ref =
    do m_creature_where <- liftM extractLocation $ dbWhere creature_ref
       flip (maybe (return [])) m_creature_where $ \(creature_position :: Position,plane_ref :: PlaneRef) ->
           do contents <- liftM (mapMaybe coerceLocation) $ dbGetContents plane_ref
              return $ map entity $ filter ((== creature_position) . location) contents
       
