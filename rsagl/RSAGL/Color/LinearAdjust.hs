module RSAGL.Color.LinearAdjust
    (LinearAdjustment,
     adjustColor,
     maximize,
     minimize,
     clamp)
    where

import Data.Maybe
import RSAGL.Types
import RSAGL.Color.ColorSpace

type LinearAdjustment = LinearMetric -> RSdouble

-- | Manipulate an arbitrary component of a color.
adjustColor :: (ExportColorCoordinates c, ImportColorCoordinates c') =>
               ColorChannel -> LinearAdjustment -> c -> c'
adjustColor chan f c = transformColor $ linear_color_function metric (f metric)
    where metric = viewChannel chan c

-- | Increase a channel to a maximum in-gamut value.
maximize :: LinearAdjustment
maximize metric = maybe (linear_value metric) snd $
                  linear_gamut_bounds metric

-- | Decrease a channel to it's minimum in-gamut value.
minimize :: LinearAdjustment
minimize metric = maybe (linear_value metric) fst $
                  linear_gamut_bounds metric

-- | Clamp a channel to the gamut.
clamp :: LinearAdjustment
clamp metric = max lo . min hi $ linear_value metric
    where (lo,hi) = fromMaybe (linear_value metric,linear_value metric) $
                              linear_gamut_bounds metric

