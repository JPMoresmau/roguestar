{-# LANGUAGE FlexibleContexts #-}

module Contact
    (findContacts,
     ContactMode(..),
     ContactModeType(..))
    where

import Position
import Facing
import DB
import CreatureData
import Control.Monad
import Plane
import Data.Ord
import Data.List as List
import Data.Maybe

-- | 'Touch' contacts are on the same or facing square as the subject.
-- 'Line' contacts are on any point starting on the same square and anywhere directly along a line traced in the
-- facing direction, out to infinity.  'Area' contacts lie inside a circle of radius 7, centered 7 squares in the
-- facing direction.  Use 'Area' 'Here' for a circle centerd on the subject.
data ContactMode = Touch | Line | Area

class ContactModeType a where
    contactMode :: a -> ContactMode

instance ContactModeType ContactMode where
    contactMode = id

instance ContactModeType CreatureInteractionMode where
    contactMode Unarmed = Touch
    contactMode Melee = Touch
    contactMode Ranged = Line
    contactMode Splash = Area

-- | Find contacts to a reference.  The result is sorted by from closest to farthest from the subject, except in the case
-- of area contacts, which are sorted from the center of the area.  The subject is never a contact of itself.
findContacts :: (DBReadable db,ReferenceType x,GenericReference a S,ContactModeType c) => c -> Reference x -> Facing -> db [a]
findContacts contact_mode attacker_ref face =
    do m_l <- liftM (fmap location) $ getPlanarLocation attacker_ref
       let testF pos x = case contactMode contact_mode of
               Touch -> location x == (offsetPosition (facingToRelative face) pos) || location x == pos
               Line -> isFacing (pos,face) $ location x
               Area -> distanceBetweenSquared (offsetPosition (facingToRelative7 face) pos) (location x) < 49
           center_pos pos = case contactMode contact_mode of
               Area -> offsetPosition (facingToRelative7 face) pos
               _ -> pos
       flip (maybe $ return []) m_l $ \(plane_ref,pos) ->
           liftM (mapMaybe fromLocation .
	          sortBy (comparing (distanceBetweenSquared (center_pos pos) . location)) .
	          filter ((/= generalizeReference attacker_ref) . entity) . 
	          filter (testF pos)) $ 
		      dbGetContents plane_ref

