\section{Sky}

Generates a realistic sky.

\begin{code}
{-# LANGUAGE PatternGuards #-}
module RSAGL.Extras.Sky
    (Atmosphere,
     SkyFilter,
     AtmosphereComposition(..),
     AtmosphereLayer(..),
     rawSkyFilter,
     dynamicSkyFilter,
     earth_atmosphere,
     mars_atmosphere,
     atmosphereAbsorbtion,
     atmosphereScattering,
     atmosphereScatteringMaterial)
    where

import RSAGL.Scattering
import RSAGL.Ray
import RSAGL.RayTrace
import RSAGL.Vector
import RSAGL.Color
import RSAGL.Angle
import RSAGL.ApplicativeWrapper
import Data.Monoid
import Data.Maybe as Maybe
import Data.List as List
import Data.Ord
import qualified RSAGL.Model as Model
import Control.Monad
import System.Random
import RSAGL.Model hiding (sphere)
\end{code}

\begin{code}
earth_atmosphere :: Atmosphere
earth_atmosphere = [
    AtmosphereLayer Air                      0.750  9.0e-4,
    AtmosphereLayer Vapor                    0.250  2.5e-4,
    AtmosphereLayer (Dust $ rgb 0.5 0.5 0.5) 0.001  2.0e-4]

mars_atmosphere :: Atmosphere
mars_atmosphere = [
    AtmosphereLayer Air                       0.05 2.5e-3,
    AtmosphereLayer (Dust $ rgb 0.8 0.25 0.1) 0.35 1.5e-3 ]

type SkyFilter = (Vector3D -> RGB) -> Maybe (RGB -> RGB)
type Atmosphere = [AtmosphereLayer]

data AtmosphereComposition = Air | Vapor | Dust RGB | Fog RGB
data AtmosphereLayer = AtmosphereLayer { 
    atmosphere_composition :: AtmosphereComposition,
    atmosphere_thickness :: Double,
    atmosphere_altitude :: Double }

rawSkyFilter :: SkyFilter
rawSkyFilter = const $ Just id

dynamicSkyFilter :: Double -> Double -> SkyFilter
dynamicSkyFilter max_black min_white origF = case () of
                                () | min_color > 0 -> Just $ filterRGBLinear (gray min_color) (gray max_color)
                                () | otherwise -> Nothing
    where max_color = foldr (\(RGB r g b) x -> max (max r g) (max b x)) min_white cs
	  min_color = foldr (\(RGB r g b) x -> minimum $ filter (/= 0) [r,g,b,x]) max_black cs
          randomCoordinates i = (map $ \x -> 2 * (if x >= 0.5 then 0.5 - x else x)) $ randoms (mkStdGen i)
          cs = take 200 $ map origF $ filter ((\x -> x > 0 && x <= 1) . vectorLength) $ zipWith3 Vector3D
		   (randomCoordinates 1305) (randomCoordinates 2543) (randomCoordinates 6037)

atmosphereLayerToScatteringModel :: AtmosphereLayer -> Scattering
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Air }) = 
     rayleigh (atmosphere_altitude l / atmosphere_thickness l) rayleigh_sky
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Vapor }) = mconcat [
    elasticOmnidirectionalScatter (atmosphere_altitude l / atmosphere_thickness l) (gray $ meanBrightness rayleigh_sky),
    elasticForwardScatter (atmosphere_altitude l / atmosphere_thickness l) (fromDegrees 30) (gray $ meanBrightness rayleigh_sky)]
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Dust c }) = 
    dust (realToFrac $ atmosphere_altitude l / atmosphere_thickness l) c
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Fog c }) = 
    fog (realToFrac $ atmosphere_altitude l / atmosphere_thickness l) c

{-# INLINE castSkyRay #-}
castSkyRay :: (Geometry g) => g -> a -> (Point3D -> Point3D -> a) -> Ray3D -> a
castSkyRay test_sphere a f r = case map (sv3d_position . snd) $ sortBy (comparing fst) $ filter ((> 0) . fst) $ testRay r test_sphere of
                                        [] -> a
			                [p_far] -> f (ray_endpoint r) p_far
			                [p_near,p_far] -> f p_near p_far
			                _ -> error "castSkyRay: unexpected case"

atmosphereLayerAbsorbtion :: AtmosphereLayer -> Ray3D -> RGB
atmosphereLayerAbsorbtion l r = castSkyRay (sphere origin_point_3d (1 + atmosphere_altitude l)) (gray 1) absorbF r
    where absorbF p_near p_far = postFilter $ traceAbsorbtion (const $ scattering_model) linearSamples p_near p_far 1
          postFilter = case atmosphere_composition l of
	                    Air -> maximizeRGB
			    _ -> id
          scattering_model = atmosphereLayerToScatteringModel l

atmosphereLayerScattering :: AtmosphereLayer -> (Vector3D,RGB) -> Ray3D -> RGB
atmosphereLayerScattering l (sun_vector,sun_color) r = castSkyRay (sphere origin_point_3d (1 + atmosphere_altitude l)) (gray 0) scatterF r
    where scatterF p_near p_far = fst $ traceScattering (const scattering_model) 
              (\p -> (sun_vector,scaleRGB (lightingF p) sun_color)) adaptiveSamples p_near p_far $
                  round $ max 20 $ (* 800) $ toRotations $ angleBetween (Vector3D 0 1 0) (ray_vector r)
          scattering_model = achromaticAbsorbtion $ atmosphereLayerToScatteringModel l
	  lightingF p = realToFrac $ castSkyRay UnitSphere 1 
	                                        (\p_near p_far -> max 0 $ sqrt (atmosphere_altitude l) - 1 + sqrt (4 - distanceBetween p_near p_far ** 2) / 2)
				                (Ray3D p sun_vector)

atmosphereAbsorbtion :: Atmosphere -> Point3D -> Vector3D -> RGB
atmosphereAbsorbtion atm p v = foldr filterRGB (gray 1) $ map ($ Ray3D p v) absorbFs
    where absorbFs = map atmosphereLayerAbsorbtion atm

atmosphereScattering :: Atmosphere -> [(Vector3D,RGB)] -> Point3D -> Vector3D -> RGB
atmosphereScattering atm_ suns p v_ = foldr addRGB (gray 0) $ map ($ v_) scatterFs
     where atm = reverse $ sortBy (comparing atmosphere_altitude) atm_
           scatterFs = 
              do (this_layer,sun_absorbtion_layers,post_absorbtion_layers) <- zip3 atm (inits atm) (drop 1 $ tails atm)
		 let sunAbsorbF = atmosphereAbsorbtion sun_absorbtion_layers
		 let postAbsorbF = atmosphereAbsorbtion post_absorbtion_layers
		 this_sun <- suns
                 return $ \v -> filterRGB (postAbsorbF p v) $ 
		                    atmosphereLayerScattering this_layer 
		                        (fst this_sun,
				         filterRGB (sunAbsorbF p $ fst this_sun) $ snd this_sun) $ Ray3D p v

atmosphereScatteringMaterial :: Atmosphere -> [(Vector3D,RGB)] -> SkyFilter -> MaterialM attr ()
atmosphereScatteringMaterial [] _ _ = return ()
atmosphereScatteringMaterial _ suns _ | all ((== 0) . maxRGB . snd) suns = return ()
atmosphereScatteringMaterial atm suns sky_filter = material $ 
    do filtering $ ApplicativeWrapper $ Left $
           \(SurfaceVertex3D p _) -> atmosphereAbsorbtion atm (Point3D 0 1 0) (vectorToFrom p origin_point_3d)
       case m_skyFilterF of
           Just skyFilterF -> emissive $ ApplicativeWrapper $ Left $ 
	       \(SurfaceVertex3D p _) -> skyFilterF $ scatteringF (vectorToFrom p origin_point_3d)
	   Nothing -> return ()
    where scatteringF = atmosphereScattering atm suns (Point3D 0 1 0)
          m_skyFilterF = sky_filter scatteringF
\end{code}
