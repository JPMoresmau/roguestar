{-# LANGUAGE PatternGuards #-}
-- | Generate a realistic sky.
module RSAGL.Extras.Sky
    (Atmosphere,
     SkyFilter,
     AtmosphereComposition(..),
     AtmosphereLayer(..),
     rawSkyFilter,
     dynamicSkyFilter,
     earth_atmosphere,
     atmosphereAbsorbtion,
     atmosphereScattering,
     absorbtionFilter,
     atmosphereScatteringMaterial)
    where

import RSAGL.Math.Ray
import RSAGL.RayTrace.RayTrace
import RSAGL.RayTrace.Scattering
import RSAGL.Math.Vector
import RSAGL.Color
import RSAGL.Math.Angle
import RSAGL.Auxiliary.ApplicativeWrapper
import Data.Monoid
import Data.List as List
import Data.Ord
import System.Random
import RSAGL.Modeling.Model hiding (sphere)
import RSAGL.Math.Types
import RSAGL.Math.AbstractVector

-- | An atmosphere that is fairly typical of the earth.
earth_atmosphere :: Atmosphere
earth_atmosphere = [
    AtmosphereLayer Air                      0.750  9.0e-4,
    AtmosphereLayer Vapor                    0.250  2.5e-4,
    AtmosphereLayer (Dust $ rgb 0.5 0.5 0.5) 0.001  2.0e-4]

-- | An adaptive color filter, used to set adaptive white and black points.
-- Returns 'Nothing' if the result is a constant black.
type SkyFilter = (Vector3D -> RGB) -> Maybe (RGB -> RGB)

-- | An atmosphere, consisting of zero or more layers of different composition.
type Atmosphere = [AtmosphereLayer]

-- | A specific scattering model for an 'AtmosphereLayer'
data AtmosphereComposition = 
    -- | Uses Rayleigh scattering, as though an oxygen-nitrogen atmosphere.
    Air
    -- | Uses Mie scattering (approximate) to give an effect similar to what we would expect
    -- from some kind of suspended water vapor.
  | Vapor 
    -- | Macroscopic colored dust spheres.
  | Dust RGB 
    -- | Unrealistic colored fog, might be appropriate for some kind of poison gas atmosphere.
  | Fog RGB

-- | A single layer of atmosphere.
data AtmosphereLayer = AtmosphereLayer { 
    -- | Represents what substance this layer of atmosphere is made of.
    atmosphere_composition :: AtmosphereComposition,
    -- | Represents the optical thickness of this layer looking straight up.  That is,
    -- if you reduce the altitude but hold the thickness constant, the layer will be
    -- essentially unchanged in the vertical direction.  A typical value is 1.0.
    atmosphere_thickness :: RSdouble,
    -- | The altitude to the edge of this atmosphere layer, where 1.0 is the diameter of the planet.  
    -- Lowering the altitude actually increases the density, and vice-versa, so double or halve the thickness
    -- and altitude at the same time.  A typical value is 1e-4.
    atmosphere_altitude :: RSdouble }

-- | A 'SkyFilter' that just passes through the raw RGB values, essentially, 'Prelude.id'.
rawSkyFilter :: SkyFilter
rawSkyFilter = const $ Just id

-- | A 'SkyFilter' that takes a maximum black point and a minimum white point, and applies
-- these to black and white points determined by probabalistic means, and then generates a
-- linear filter based on those points.  For well chosen parameters this will hopefully 
-- produce an appealing sky at any time of day or twilight.
--
dynamicSkyFilter :: RSdouble -> RSdouble -> SkyFilter
dynamicSkyFilter max_black min_white origF = case () of
                                () | min_color > 0 -> Just $ filterRGBLinear (grayscale min_color) (grayscale max_color)
                                () | otherwise -> Nothing
    where max_color = foldr (\(RGB r g b) x -> max (max r g) (max b x)) min_white cs
	  min_color = foldr (\(RGB r g b) x -> minimum $ filter (/= 0) [r,g,b,x]) max_black cs
          randomCoordinates i = (map $ \x -> 2 * (if x >= 0.5 then 0.5 - x else x)) $ map f2f $ (randoms (mkStdGen i) :: [Double])
          cs = take 200 $ map origF $ filter ((\x -> x > 0 && x <= 1) . vectorLength) $ zipWith3 Vector3D
		   (randomCoordinates 1305) (randomCoordinates 2543) (randomCoordinates 6037)

-- | Generate a low level 'Scattering' model directly from an 'AtmosphereLayer'.
atmosphereLayerToScatteringModel :: AtmosphereLayer -> Scattering
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Air }) = 
     rayleigh (atmosphere_altitude l / atmosphere_thickness l) rayleigh_sky
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Vapor }) = mconcat [
    elasticOmnidirectionalScatter (atmosphere_altitude l / atmosphere_thickness l)
                                  (grayscale $ linear_value $ viewChannel channel_brightness rayleigh_sky),
    elasticForwardScatter (atmosphere_altitude l / atmosphere_thickness l) (fromDegrees 30)
                          (grayscale $ linear_value $ viewChannel channel_brightness rayleigh_sky)]
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Dust c }) = 
    dust (f2f $ atmosphere_altitude l / atmosphere_thickness l) c
atmosphereLayerToScatteringModel l@(AtmosphereLayer { atmosphere_composition = Fog c }) = 
    fog (f2f $ atmosphere_altitude l / atmosphere_thickness l) c

-- | Cast a ray that can intersect a geometry at exactly two or zero points, given a default value
-- for the zero-intersection case and a function to determine a value for the
-- two-intersection case.  In a one-intersection case, it is assumed that the ray terminates in the
-- interior of the geometry, and the endpoint of the ray is used as a point of intersection.
--
-- TODO: should this or a generalization of this be moved to RSAGL.RayTrace?
--
{-# INLINE castSkyRay #-}
castSkyRay :: (Geometry g) => g -> a -> (Point3D -> Point3D -> a) -> Ray3D -> a
castSkyRay test_sphere a f r = case map (sv3d_position . snd) $ sortBy (comparing fst) $ filter ((> 0) . fst) $ testRay r test_sphere of
                                        [] -> a
			                [p_far] -> f (ray_endpoint r) p_far
			                [p_near,p_far] -> f p_near p_far
			                _ -> error "castSkyRay: unexpected case"

-- | Calculate the amount of absorbtion along a specific ray inside a single 'AtmosphereLayer'.
atmosphereLayerAbsorbtion :: AtmosphereLayer -> Ray3D -> RGB
atmosphereLayerAbsorbtion l r = castSkyRay (sphere origin_point_3d (1 + atmosphere_altitude l)) (grayscale 1) absorbF r
    where absorbF p_near p_far = postFilter $ traceAbsorbtion (const $ scattering_model) linearSamples p_near p_far 1
          postFilter = case atmosphere_composition l of
	                    Air -> adjustColor channel_value maximize
			    _ -> id
          scattering_model = atmosphereLayerToScatteringModel l

-- | Calculate the amount of scattering along a specific ray inside a single 'AtmosphereLayer' given the position and color of a single sun.
atmosphereLayerScattering :: AtmosphereLayer -> (Vector3D,RGB) -> Ray3D -> RGB
atmosphereLayerScattering l (sun_vector,sun_color) r = castSkyRay (sphere origin_point_3d (1 + atmosphere_altitude l)) (grayscale 0) scatterF r
    where scatterF p_near p_far = fst $ traceScattering (const scattering_model) 
              (\p -> (sun_vector,scalarMultiply (lightingF p) sun_color)) adaptiveSamples p_near p_far $
                  round $ max 20 $ (* 800) $ toRotations $ angleBetween (Vector3D 0 1 0) (ray_vector r)
          scattering_model = achromaticAbsorbtion $ atmosphereLayerToScatteringModel l
	  lightingF p = f2f $ castSkyRay UnitSphere 1 
	                                        (\p_near p_far -> max 0 $ sqrt (atmosphere_altitude l) - 1 + sqrt (4 - distanceBetween p_near p_far ** 2) / 2)
				                (Ray3D p sun_vector)

-- | Aggrigated absorbtion from multiple 'AtmosphereLayers'.
atmosphereAbsorbtion :: Atmosphere -> Point3D -> Vector3D -> RGB
atmosphereAbsorbtion atm p v = foldr filterRGB (grayscale 1) $ map ($ Ray3D p v) absorbFs
    where absorbFs = map atmosphereLayerAbsorbtion atm

-- | Aggrigated scattering from multiple 'AtmosphereLayers' and multiple suns.
atmosphereScattering :: Atmosphere -> [(Vector3D,RGB)] -> Point3D -> Vector3D -> RGB
atmosphereScattering atm_ suns p v_ = foldr add (grayscale 0) $ map ($ v_) scatterFs
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

-- | Takes a filter color and modifies it on a logarithmic scale.  Helps when dealing with very dense color filters.
-- In particular, atmosphereScatteringMaterial uses this.
--
absorbtionFilter :: RGB -> RGB
absorbtionFilter c =
    let maximal_rgb = linear_value $ viewChannel channel_value c
        maximized_rgb = adjustColor channel_value maximize c
        in scalarMultiply
               (recip $ 1 + (abs $ log maximal_rgb / log 2))
               maximized_rgb

-- | Generate a material for a sky sphere.  This material includes both scattering and absorbtion information.
-- The material assumes the origin as the eye point, tracing to the geometric point at each vertex.  Therefore,
-- this material need not be applied to an exact sphere.
--
atmosphereScatteringMaterial :: Atmosphere -> [(Vector3D,RGB)] -> SkyFilter -> MaterialM attr ()
atmosphereScatteringMaterial [] _ _ = return ()
atmosphereScatteringMaterial _ suns _ |
    all ((== 0) . linear_value . viewChannel channel_value . snd) suns =
        return ()
atmosphereScatteringMaterial atm suns sky_filter = material $
    do filtering $ ApplicativeWrapper $ Left $
           \(SurfaceVertex3D p _) -> absorbtionFilter $ atmosphereAbsorbtion atm (Point3D 0 1 0) (vectorToFrom p origin_point_3d)
       case m_skyFilterF of
           Just skyFilterF -> emissive $ ApplicativeWrapper $ Left $ 
	       \(SurfaceVertex3D p _) -> skyFilterF $ scatteringF (vectorToFrom p origin_point_3d)
	   Nothing -> return ()
    where scatteringF = atmosphereScattering atm suns (Point3D 0 1 0)
          m_skyFilterF = sky_filter scatteringF
