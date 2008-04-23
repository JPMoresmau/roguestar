{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

--
-- Perception is essentially a catalogue of information that can be
-- observed from a creatures-eye-view, i.e. information that
-- is legal for a human agent or ai agent to have.
--

module Perception
    (DBPerception,
     whoAmI,
     runPerception,
     visibleObjects,
     myFaction,
     Perception.getCreatureFaction,
     whereAmI,
     myPosition,
     whereIs,
     Perception.roll)
    where

import Control.Monad.Reader
import Control.Monad
import DB
import FactionData
import Creature
import PlaneVisibility
import Data.Maybe
import Facing
import Dice

newtype (DBReadable db) => DBPerception db a = DBPerception { fromPerception :: (ReaderT CreatureRef db a) }

instance (DBReadable db) => Monad (DBPerception db) where
    (DBPerception a) >>= m = DBPerception $ a >>= (\x -> case m x of {(DBPerception b) -> b})
    return = DBPerception . return

liftDB :: (DBReadable db) => (forall m. DBReadable m => m a) -> DBPerception db a
liftDB actionM = DBPerception $ lift actionM

whoAmI :: (DBReadable db) => DBPerception db CreatureRef
whoAmI = DBPerception $ ask

runPerception :: (DBReadable db) => CreatureRef -> (forall m. DBReadable m => DBPerception m a) -> db a
runPerception creature_ref perception = dbSimulate $ runReaderT (fromPerception perception) creature_ref

visibleObjects :: (DBReadable db,LocationType a,LocationType b) => DBPerception db [Location S a b]
visibleObjects =
    do me <- whoAmI
       faction <- myFaction
       liftDB $ maybe (return []) (dbGetVisibleObjectsForFaction faction) =<< liftM extractLocation (dbWhere me)

myFaction :: (DBReadable db) => DBPerception db Faction
myFaction = Perception.getCreatureFaction =<< whoAmI

getCreatureFaction :: (DBReadable db) => CreatureRef -> DBPerception db Faction
getCreatureFaction creature_ref = liftDB $ Creature.getCreatureFaction creature_ref

whereAmI :: (DBReadable db) => DBPerception db (Facing,Position)
whereAmI = liftM (fromMaybe (error "whereAmI: I'm not on a plane") . extractLocation) $ whereIs =<< whoAmI

myPosition :: (DBReadable db) => DBPerception db Position
myPosition = liftM snd whereAmI

whereIs :: (DBReadable db) => Reference a -> DBPerception db (Location S (Reference a) ())
whereIs ref = liftDB $ dbWhere ref

roll :: (DBReadable db) => [a] -> DBPerception db a
roll xs = liftDB $ Dice.roll xs
