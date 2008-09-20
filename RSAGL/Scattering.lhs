\section{Scattering}

Scattering models the behavior or light interacting with dust and air molecules.

\begin{code}
module RSAGL.Scattering
    (Scattering(..),
     absorbtionOverDistance,
     achromaticAbsorbtion,
     withoutAbsorbtion,
     withoutScattering,
     emissionOverDistance,
     traceScattering,
     traceAbsorbtion,
     dust,
     fog,
     rayleigh_sky,
     rayleigh,
     elasticBackScatter,
     elasticForwardScatter,
     elasticOmnidirectionalScatter)
    where

import RSAGL.Vector
import RSAGL.Color
import RSAGL.Angle
import RSAGL.Interpolation
import RSAGL.Auxiliary
import Data.Monoid
\end{code}

\section{Scattering}

\begin{code}
data Scattering = Scattering {
    scattering_absorb :: RGB,
    scattering_scatter :: Angle -> RGB }

adjustDistance :: Double -> Scattering -> Scattering
adjustDistance d s = Scattering {
    scattering_absorb = mapRGB (** (recip d)) $ scattering_absorb s,
    scattering_scatter = scaleRGB (recip d) . scattering_scatter s }

mapAbsorbtion :: (RGB -> RGB) -> Scattering -> Scattering
mapAbsorbtion f s = s { scattering_absorb = f $ scattering_absorb s }

mapScattering :: (RGB -> RGB) -> Scattering -> Scattering
mapScattering f s = s { scattering_scatter = f . scattering_scatter s }

achromaticAbsorbtion :: Scattering -> Scattering
achromaticAbsorbtion = mapAbsorbtion (gray . meanBrightness)

withoutAbsorbtion :: Scattering -> Scattering
withoutAbsorbtion = mapAbsorbtion (const $ gray 1)

withoutScattering :: Scattering -> Scattering
withoutScattering = mapScattering (const $ gray 0)

absorbtionOverDistance :: Double -> RGB -> RGB
absorbtionOverDistance trace_distance absorb_rgb = mapRGB (** trace_distance) absorb_rgb

emissionOverDistance :: Double -> RGB -> RGB
emissionOverDistance trace_distance emit_rgb = scaleRGB trace_distance emit_rgb

traceScattering :: (Point3D -> Scattering) -> (Point3D -> (Vector3D,RGB)) -> Point3D -> Point3D -> Samples RGB
traceScattering scatteringF lightingF source destination number_of_samples = 
    foldr addRGB (gray 0) $ sampleScattering scatteringF lightingF source destination number_of_samples

sampleScattering :: (Point3D -> Scattering) -> (Point3D -> (Vector3D,RGB)) -> Point3D -> Point3D -> Samples [RGB]
sampleScattering scatteringF lightingF source destination number_of_samples = zipWith 
        (\absorb_color scatter_color -> filterRGB absorb_color scatter_color) 
        sample_absorbtions sample_scatterings
    where vector_to_viewer = vectorToFrom source destination
          samples = linearSamples source destination number_of_samples
          sample_scatterings = flip map samples (\(this_point,this_distance) -> 
	      let s = scatteringF this_point
	          (light_vector,light_color) = lightingF this_point
		  scattering_angle = angleBetween vector_to_viewer light_vector
		  scatter_color = scattering_scatter s scattering_angle
	          in emissionOverDistance this_distance $ filterRGB scatter_color light_color)
          sample_absorbtions = scanl1 filterRGB $ sampleAbsorbtion scatteringF source destination number_of_samples

traceAbsorbtion :: (Point3D -> Scattering) -> Point3D -> Point3D -> Samples RGB
traceAbsorbtion scatteringF source destination samples = 
    foldr filterRGB (gray 1) $ sampleAbsorbtion scatteringF source destination samples

sampleAbsorbtion :: (Point3D -> Scattering) -> Point3D -> Point3D -> Samples [RGB]
sampleAbsorbtion scatteringF source destination samples = sample_absorbtions
    where sample_points = linearSamples source destination samples
	  sample_absorbtions = map (\(p,d) -> absorbtionOverDistance d $ scattering_absorb $ scatteringF p) sample_points
\end{code}

\subsection{Sampling}

\texttt{linearSamples} takes samples evenly spaced along a ray.

\begin{code}
type Samples x = Integer -> x

linearSamples :: Point3D -> Point3D -> Integer -> [(Point3D,Double)]
linearSamples source destination samples = zip sample_points $ repeat distance_per_sample
    where sample_points = map (flip lerp (source,destination)) $ zeroToOne samples
          distance_per_sample = realToFrac $ distanceBetween source destination / (fromInteger samples)
\end{code}

\subsection{Specific Scattering Functions}

\begin{code}
dust :: Double -> RGB -> Scattering
dust d c = adjustDistance d $ Scattering {
    scattering_absorb = gray 0.5,
    scattering_scatter = flip scaleRGB c . (*0.5) . (1-) . (*2) . realToFrac . toRotations_ }

fog :: Double -> RGB -> Scattering
fog d c = adjustDistance d $ Scattering {
    scattering_absorb = c,
    scattering_scatter = const c }

rayleigh_sky :: RGB
rayleigh_sky = rgb 0.06 0.10 0.23 

rayleigh :: Double -> RGB -> Scattering
rayleigh d c = adjustDistance d $ Scattering {
    scattering_absorb = invertRGB c,
    scattering_scatter = \theta -> scaleRGB ((1 + realToFrac (cosine theta)^2)*0.75 ) c }

elasticBackScatter :: Double -> Angle -> RGB -> Scattering
elasticBackScatter d a c_ = adjustDistance d $ Scattering {
    scattering_absorb = invertRGB c,
    scattering_scatter = \theta -> scaleRGB (realToFrac $ max 0 $ n*(r - (toRadians_ theta)^2)) c }
        where r_ = toRadians a
              r = r_^2
	      n = recip $ (1 + r/2 - sin(r_)*r_ - cos(r_))
	      c = scaleRGB (realToFrac $ (toRotations a * 4)^2) c_

elasticForwardScatter :: Double -> Angle -> RGB -> Scattering
elasticForwardScatter d a c = s {
    scattering_scatter = scattering_scatter s . reverseAngle }
        where s = elasticBackScatter d a c

elasticOmnidirectionalScatter :: Double -> RGB -> Scattering
elasticOmnidirectionalScatter d c = adjustDistance d $ Scattering {
    scattering_absorb = invertRGB c,
    scattering_scatter = const c }
\end{code}

\subsection{Scattering Monoid}

\begin{code}
instance Monoid Scattering where
    mempty = Scattering {
        scattering_absorb = gray 1.0,
        scattering_scatter = const $ gray 0 }
    x `mappend` y = Scattering {
        scattering_absorb = scattering_absorb x `filterRGB` scattering_absorb y,
        scattering_scatter = \u -> scattering_scatter x u `addRGB` scattering_scatter y u }
    mconcat [] = mempty
    mconcat [x] = x
    mconcat xs = mconcat a `mappend` mconcat b
        where (a,b) = splitAt (length xs `div` 2) xs
\end{code}
