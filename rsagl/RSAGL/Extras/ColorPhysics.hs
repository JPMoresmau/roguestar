module RSAGL.Extras.ColorPhysics
    (plancksLaw,
     blackBody,
     blackBodyRGB,
     spectralRGB) where

import RSAGL.Color
import RSAGL.Math.AbstractVector
import RSAGL.Auxiliary.Auxiliary
import RSAGL.Math.Interpolation
import RSAGL.Types

-- | Evaluates planck's law respecting blackbody radiation.
-- Accepts temperature in Kelvins (K) and wavelength in nanometers (nm).
plancksLaw :: RSdouble -> RSdouble -> RSdouble
plancksLaw t w = (8 * pi * h * c / w^5) / (exp (h*c/w/k/t) - 1)
    where h = 6.626070e-34  -- (J*s)
          c = 2.997924e17   -- (nm/s)
	  k = 1.3806504e-23 -- (J/K)

-- | Indicates the intensity of black body radiation in terms of temperature and wavelength, as percieved by the human eye
-- with a white point at which all wavelengths equal 1.0 at 5800K.
-- Accepts temperature in Kelvins (K) and wavelength in nanometers (nm).
blackBody :: RSdouble -> RSdouble -> RSdouble
blackBody t w = plancksLaw t w / plancksLaw 5800 w

-- | Indicates the percieved color of a black body radiator, where @rgb 1.0 1.0 1.0@ is the white point representing 5800K.
-- Accepts temperature in Kelvins (K).  It is suggested to use 'maximizeRGB' or some other filter as very dark or overbright
-- colors are easily generated from this function.  In particular @maximizeRGB . blackBodyRGB@ tends to approach roughly
-- @rgb 0.0 0.0 1.0@ for very low temperatures and roughly @rgb 0.50 0.53 1.0@ for very high temperatures.
blackBodyRGB :: RSdouble -> RGB
blackBodyRGB t = spectralRGB (blackBody t) 

-- | Interprets a spectral function as an 'RGB' color by sampling in the red, green, blue, and indigo wavelengths.
-- This is pretty rough, and actually interprets monochromatic spectral yellow or monochromatic spectral cyan as
-- black, for example.  It also does not take into account the relative responsiveness of the human eye to
-- different wavelengths, so passing @'plancksLaw' 5800@ directly to this function results in bright green.
spectralRGB :: (RSdouble -> RSdouble) -> RGB
spectralRGB f = rgb
    (abstractAverage $ (map (f . (flip lerp (625,740))) $ zeroToOne 5) ++ [f 415])
    (abstractAverage $  map (f . (flip lerp (520,570))) $ zeroToOne 5)
    (abstractAverage $  map (f . (flip lerp (440,490))) $ zeroToOne 5)
