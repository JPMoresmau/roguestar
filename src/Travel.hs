module Travel
    (stepCreature,
     jumpCreature,
     turnCreature)
    where

import Control.Monad.Maybe
import Facing
import DB
import Plane
import Data.Maybe
import Control.Monad
import Control.Monad.Trans

walkCreature :: (DBReadable db) => Facing -> (Integer,Integer) -> 
                                     Location m CreatureRef () -> db (Location m CreatureRef ())
walkCreature face (x',y') l = liftM (fromMaybe l) $ runMaybeT $
    do (plane_ref,Position (x,y)) <- MaybeT $ return $ extractLocation l
       let standing = Standing { standing_plane = plane_ref,
                                 standing_position = Position (x+x',y+y'),
                                 standing_facing = face } 
       flip unless (fail "") =<< (lift $ isTerrainPassable plane_ref (entity l) $ standing_position standing)
       return $ generalizeLocation $ toStanding standing l

stepCreature :: (DBReadable db) => Facing -> Location m CreatureRef () -> db (Location m CreatureRef ())
stepCreature face = walkCreature face (facingToRelative face)

jumpCreature :: (DBReadable db) => Facing -> Location m CreatureRef () -> db (Location m CreatureRef ())
jumpCreature face = walkCreature face (facingToRelative7 face)

turnCreature :: (DBReadable db) => Facing -> Location m CreatureRef () -> db (Location m CreatureRef ())
turnCreature face = walkCreature face (0,0)

