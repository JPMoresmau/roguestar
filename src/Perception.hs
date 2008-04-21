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
     creatureFaction,
     whereAmI,
     myPosition,
     whereIs)
    where

import Control.Monad.Reader
import Control.Monad
import DB
import FactionData
import Creature
import PlaneVisibility
import Data.Maybe
import Facing

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
       faction <- liftDB $ dbGetCreatureFaction me
       liftDB $ maybe (return []) (dbGetVisibleObjectsForFaction faction) =<< liftM extractLocation (dbWhere me)

myFaction :: (DBReadable db) => DBPerception db Faction
myFaction = creatureFaction =<< whoAmI

creatureFaction :: (DBReadable db) => CreatureRef -> DBPerception db Faction
creatureFaction creature_ref = liftDB $ dbGetCreatureFaction creature_ref

whereAmI :: (DBReadable db) => DBPerception db (Facing,Position)
whereAmI = liftM (fromMaybe (error "whereAmI: I'm not on a plane") . extractLocation) $ whereIs =<< whoAmI

myPosition :: (DBReadable db) => DBPerception db Position
myPosition = liftM snd whereAmI

whereIs :: (DBReadable db) => Reference a -> DBPerception db (Location S (Reference a) ())
whereIs ref = liftDB $ dbWhere ref
