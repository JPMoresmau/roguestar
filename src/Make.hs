{-# LANGUAGE FlexibleInstances #-}
module Make
    (PrepareMake,
     prepare_make,
     isFinished,
     needsKind,
     needsChromalite,
     needsMaterial,
     needsGas,
     MakeWith(..))
    where

import ToolData
import DBData
import Substances

-- | Multi-step process for gathering the materials to make something.
data PrepareMake = PrepareMake {
    m_device_kind :: (Maybe (DeviceKind,Integer)),
    m_chromalite :: (Maybe (Chromalite,ToolRef)),
    m_material :: (Maybe (Material,ToolRef)),
    m_gas :: (Maybe (Gas,ToolRef)) } deriving (Read,Show)

-- | An empty prepare_make.
prepare_make :: PrepareMake
prepare_make = PrepareMake Nothing Nothing Nothing Nothing

isFinished :: PrepareMake -> Bool
isFinished (PrepareMake (Just _) (Just _) (Just _) (Just _)) = True
isFinished _ = False

needsKind :: PrepareMake -> Bool
needsKind (PrepareMake Nothing _ _ _) = True
needsKind _ = False

needsChromalite :: PrepareMake -> Bool
needsChromalite (PrepareMake _ Nothing _ _) = True
needsChromalite _ = False

needsMaterial :: PrepareMake -> Bool
needsMaterial (PrepareMake _ _ Nothing _) = True
needsMaterial _ = False

needsGas :: PrepareMake -> Bool
needsGas (PrepareMake _ _ _ Nothing) = True
needsGas _ = False

class MakeWith a where
    makeWith :: PrepareMake -> a -> PrepareMake

instance MakeWith (DeviceKind,Integer) where
    makeWith make_prep x = make_prep { m_device_kind = Just x }

instance (SubstanceType s) => MakeWith (s,ToolRef) where
    makeWith make_prep (x,tool_ref) = makeWithSubstance make_prep (toSubstance x,tool_ref)

makeWithSubstance :: PrepareMake -> (Substance,ToolRef) -> PrepareMake
makeWithSubstance make_prep (ChromaliteSubstance s,tool_ref) = make_prep { m_chromalite = Just (s,tool_ref) }
makeWithSubstance make_prep (MaterialSubstance s,tool_ref) = make_prep { m_material = Just (s,tool_ref) }
makeWithSubstance make_prep (GasSubstance s,tool_ref) = make_prep { m_gas = Just (s,tool_ref) }

