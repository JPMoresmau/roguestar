{-# LANGUAGE FlexibleInstances #-}
module MakeData
    (PrepareMake(..),
     prepare_make,
     isFinished,
     needsKind,
     needsChromalite,
     needsMaterial,
     needsGas,
     hasChromalite,
     hasMaterial,
     hasGas,
     MakeWith(..))
    where

import DBData
import ToolData
import Substances

-- | Multi-step process for gathering the materials to make something.
data PrepareMake = PrepareMake {
    m_device_kind :: (Maybe DeviceKind),
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

hasChromalite :: Tool -> Maybe Chromalite
hasChromalite (DeviceTool _ d) = Just $ deviceChromalite d
hasChromalite (Sphere (ChromaliteSubstance s)) = Just s
hasChromalite _ = Nothing

hasMaterial :: Tool -> Maybe Material
hasMaterial (DeviceTool _ d) = Just $ deviceMaterial d
hasMaterial (Sphere (MaterialSubstance s)) = Just s
hasMaterial _ = Nothing

hasGas :: Tool -> Maybe Gas
hasGas (DeviceTool _ d) = Just $ deviceGas d
hasGas (Sphere (GasSubstance s)) = Just s
hasGas _ = Nothing

class MakeWith a where
    makeWith :: PrepareMake -> a -> PrepareMake

instance MakeWith DeviceKind where
    makeWith make_prep x = make_prep { m_device_kind = Just x }

instance (SubstanceType s) => MakeWith (s,ToolRef) where
    makeWith make_prep (x,tool_ref) = makeWithSubstance make_prep (toSubstance x,tool_ref)

makeWithSubstance :: PrepareMake -> (Substance,ToolRef) -> PrepareMake
makeWithSubstance make_prep (ChromaliteSubstance s,tool_ref) = make_prep { m_chromalite = Just (s,tool_ref) }
makeWithSubstance make_prep (MaterialSubstance s,tool_ref) = make_prep { m_material = Just (s,tool_ref) }
makeWithSubstance make_prep (GasSubstance s,tool_ref) = make_prep { m_gas = Just (s,tool_ref) }

