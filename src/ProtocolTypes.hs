-- | ADTs corresponding to certain data tables incomming from roguestar-engine.
module ProtocolTypes
    (ProtocolType(..),
     TerrainTile(..),
     VisibleObject(..),
     WieldedObject(..),
     tableSelectTyped)
    where

import Data.Maybe
import Tables
import RSAGL.Math.Angle
import Debug.Trace

-- | 'ProtocolType' is any type that can be constructed from a row of a 'RoguestarTable'.
-- 'formatTable' is a function from bottom to the expected format of the table row.
class ProtocolType t where
    fromTable :: [TableDataFormat String Integer] -> Maybe t
    formatTable :: t -> [TableDataFormat String String]

data TerrainTile = TerrainTile {
    tt_type :: String,
    tt_xy :: (Integer,Integer) }

instance ProtocolType TerrainTile where
    formatTable = const [TDString "terrain-type",TDNumber "x",TDNumber "y"]
    fromTable [TDString t,TDNumber x,TDNumber y] = Just $ TerrainTile t (x,y)
    fromTable _ = Nothing

data VisibleObject = VisibleObject {
    vo_unique_id :: Integer,
    vo_xy :: (Integer,Integer),
    vo_facing :: BoundAngle }

instance ProtocolType VisibleObject where
    formatTable = const $ [TDNumber "object-unique-id",TDNumber "x",TDNumber "y",TDString "facing"]
    fromTable [TDNumber unique_id,TDNumber x,TDNumber y,TDString facing] = Just $
        VisibleObject unique_id (x,y) (facingToAngle facing)
    fromTable _ = Nothing

data WieldedObject = WieldedObject {
    wo_unique_id :: Integer,
    wo_creature_id :: Integer }

instance ProtocolType WieldedObject where
    formatTable = const $ [TDNumber "uid",TDNumber "creature"]
    fromTable [TDNumber unique_id,TDNumber creature_id] = Just $
        WieldedObject unique_id creature_id
    fromTable _ = Nothing

facingToAngle :: String -> BoundAngle
facingToAngle "south" = BoundAngle $ fromDegrees 0
facingToAngle "southeast" = BoundAngle $ fromDegrees 45
facingToAngle "east" = BoundAngle $ fromDegrees 90
facingToAngle "northeast" = BoundAngle $ fromDegrees 135
facingToAngle "north" = BoundAngle $ fromDegrees 180
facingToAngle "northwest" = BoundAngle $ fromDegrees 225
facingToAngle "west" = BoundAngle $ fromDegrees 270
facingToAngle "southwest" = BoundAngle $ fromDegrees 315
facingToAngle "here" = BoundAngle $ fromDegrees 0
facingToAngle s = trace ("facingToAngle: what is " ++ s ++ "?") $ BoundAngle $ fromDegrees 180

tableSelectTyped :: (ProtocolType t) => RoguestarTable -> [t]
tableSelectTyped the_table = result
    where result = mapMaybe fromTable $ tableSelectFormatted the_table the_format 
          the_format = formatTable $ (error "tableSelectTyped: undefined" :: (a -> a)) (head result)

