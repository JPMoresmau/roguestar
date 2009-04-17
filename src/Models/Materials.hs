module Models.Materials
    (
     -- | Materials by Faction
     alliance_metal,
     concordance_metal,
     concordance_dark_glass,
     concordance_bright_glass,
     cyborg_metal,
     cyborg_glow,
     -- | Material by Species
     caduceator_skin,
     encephalon_skin,
     reptilian_skin,
     reptilian_pigment,
     reptilian_specular,
     -- | Material by Energy Type
     energyColor,
     energyMaterial)
    where

import RSAGL.Modeling
import Models.LibraryData

{---------------------------------------------------------
 - Materials by Faction
 - -------------------------------------------------------}

-- Alliance Materials

alliance_metal :: Modeling ()
alliance_metal = material $
    do pigment $ pure $ scaleRGB 0.6 gold
       specular 7 $ pure $ scaleRGB 1.0 gold

-- Concordance Materials

concordance_metal :: Modeling ()
concordance_metal = material $
    do pigment $ pure slate_gray
       specular 4 $ pure lilac

concordance_dark_glass :: Modeling ()
concordance_dark_glass = material $
    do pigment $ pure black
       specular 8 $ pure eggplant

concordance_bright_glass :: Modeling ()
concordance_bright_glass = material $
    do pigment $ pure black
       emissive $ pure puce
       specular 8 $ pure eggplant

-- Cyborg Materials

cyborg_metal :: MaterialM attr ()
cyborg_metal = metallic $ pure wheat

cyborg_glow :: MaterialM attr ()
cyborg_glow = 
    do pigment $ pure blackbody
       emissive $ pure $ scaleRGB 1.0 green

{-------------------------------------------------------
 - Materials by Species
 - -----------------------------------------------------}

-- Caduceator Skins

caduceator_skin :: Modeling ()
caduceator_skin = material $ pigment $ pattern (cloudy 75 0.01) [(0.0,pure red),(0.5,pure safety_orange),(1.0,pure black)]

-- Reptilian Skins

reptilian_pigment :: ColorFunction RGB
reptilian_pigment = pattern (cloudy 75 0.1) [(0.0,pure lavender),(1.0,pure saffron)]

reptilian_specular :: ColorFunction RGB
reptilian_specular = pattern (cloudy 75 0.1) [(0.0,pure firebrick),(1.0,pure chartreuse)]

reptilian_skin :: Modeling ()
reptilian_skin = material $
    do pigment $ reptilian_pigment
       specular 5.0 $ reptilian_specular

-- Encephalon Skins

encephalon_skin :: Modeling ()
encephalon_skin = material $ pigment $ pattern (cloudy 32 0.1) [(0.0,pure sepia),(1.0,pure amethyst)]

{--------------------------------------------------------
 - Material by Energy Type
 - ------------------------------------------------------}

energyColor :: EnergyColor -> RGB
energyColor Blue = cobalt
energyColor Yellow = saffron
energyColor Red = red
energyColor Green = shamrock

energyMaterial :: EnergyColor -> Modeling ()
energyMaterial c = material $
    do pigment $ pure $ scaleRGB 0.33 $ energyColor c
       specular 1.0 $ pure $ scaleRGB 0.33 $ energyColor c
       emissive $ pure $ scaleRGB 0.33 $ energyColor c

