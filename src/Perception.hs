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
     creatureFaction)
    where

import Control.Monad.Reader
import Control.Monad
import DB
import FactionData
import Creature
import PlaneVisibility

newtype DBPerception db a = DBPerception { fromPerception :: (ReaderT CreatureRef db a) }

instance (DBReadable db) => Monad (DBPerception db) where
    (DBPerception a) >>= m = DBPerception $ a >>= (\x -> case m x of {(DBPerception b) -> b})
    return = DBPerception . return

liftDB :: (DBReadable db) => (forall m. DBReadable m => m a) -> DBPerception db a
liftDB actionM = DBPerception $ lift actionM

whoAmI :: (DBReadable db) => DBPerception db CreatureRef
whoAmI = DBPerception $ ask

runPerception :: (DBReadable db) => CreatureRef -> (forall m. DBReadable m => DBPerception m a) -> db a
runPerception creature_ref perception = dbSimulate $ runReaderT (fromPerception perception) creature_ref

visibleObjects :: (DBReadable db) => DBPerception db [Reference ()]
visibleObjects =
    do me <- whoAmI
       faction <- liftDB $ dbGetCreatureFaction me
       liftDB $ maybe (return []) (dbGetVisibleObjectsForFaction faction) =<< liftM (fmap location . toPlanarLocation) (dbWhere me)

myFaction :: (DBReadable db) => DBPerception db Faction
myFaction = creatureFaction =<< whoAmI

creatureFaction :: (DBReadable db) => CreatureRef -> DBPerception db Faction
creatureFaction creature_ref = liftDB $ dbGetCreatureFaction creature_ref
