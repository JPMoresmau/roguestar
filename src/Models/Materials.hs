module Models.Materials
    (
     -- | Materials by Faction
     treaty_metal,
     treaty_glow,
     treaty_energy_field,
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

-- Treaty Organization Materials (cyan-teal solid colors, yellow energy colors)

treaty_metal :: MaterialM attr ()
treaty_metal = material $
    do pigment $ pure camouflage_green
       specular 1 $ pure $ viridian

treaty_glow :: MaterialM attr ()
treaty_glow = material $
    do pigment $ pure black
       emissive $ pure brass

treaty_energy_field :: MaterialM attr ()
treaty_energy_field = material $
    do emissive $ pure brass
       specular 1 $ pure saffron

-- Alliance Materials  (yellow-gold solid colors, orange energy colors)

alliance_metal :: Modeling ()
alliance_metal = material $
    do pigment $ pure $ scaleRGB 0.6 gold
       specular 7 $ pure gold

-- Concordance Materials  (violet solid colors, blue energy colors)

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

-- Pirates  (green solid colors, red energy colors)

-- Utopiate (red solid colors, cyan-teal energy colors)

-- Whispers (black solid colors, white energy colors)

-- Monster (orange solid colors, violet energy colors)

-- Cyborg Materials  (white solid colors, green energy colors)

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

