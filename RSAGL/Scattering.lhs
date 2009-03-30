\section{Scattering}

Scattering models the behavior or light interacting with dust and air molecules.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module RSAGL.Scattering
    (Scattering(..),
     absorbtionOverDistance,
     achromaticAbsorbtion,
     withoutAbsorbtion,
     withoutScattering,
     emissionOverDistance,
     traceScattering,
     traceAbsorbtion,
     linearSamples,
     AdaptiveSample,
     adaptiveSamples,
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
import Data.List
\end{code}

\section{Scattering}

\begin{code}
data Scattering = Scattering {
    scattering_absorb :: RGB,
    scattering_scatter :: Angle -> RGB }
\end{code}

\texttt{adjustDistance} multiplies the distance over which a \texttt{Scattering} media has its stated effect.
For example, if medium \texttt{x} absorbs 50% of light passing through 3 units of distance, then \texttt{adjustDistance 2 x},
will absorb 50% of light passing through 6 units of distance.

\begin{code}
adjustDistance :: Double -> Scattering -> Scattering
adjustDistance d s = Scattering {
    scattering_absorb = mapRGB (** (recip d)) $ scattering_absorb s,
    scattering_scatter = scaleRGB (recip d) . scattering_scatter s }

mapAbsorbtion :: (RGB -> RGB) -> Scattering -> Scattering
mapAbsorbtion f s = s { scattering_absorb = f $ scattering_absorb s }

mapScattering :: (RGB -> RGB) -> Scattering -> Scattering
mapScattering f s = s { scattering_scatter = f . scattering_scatter s }
\end{code}

\texttt{achromaticAbsorbtion} adjusts a \texttt{Scattering} media to absorb light equally in all colors.
This is a simple way to fake a global lighting model for elastic scattering media, since without a global
model absorbed light isn't re-scattered, resulting in wrong colors.

\begin{code}
achromaticAbsorbtion :: Scattering -> Scattering
achromaticAbsorbtion = mapAbsorbtion (gray . meanBrightness)
\end{code}

\texttt{withoutAbsorbtion} removes all absorbtion from a \texttt{Scattering} media.

\begin{code}
withoutAbsorbtion :: Scattering -> Scattering
withoutAbsorbtion = mapAbsorbtion (const $ gray 1)
\end{code}

\texttt{withoutScattering} removes all scattering from a \texttt{Scattering} media.

\begin{code}
withoutScattering :: Scattering -> Scattering
withoutScattering = mapScattering (const $ gray 0)
\end{code}

\texttt{absorbtionOverDistance} takes a distance and the filter color of the absorbion media
through a distance of 1, and answers the resulting filter color of the media through that distance.

\begin{code}
absorbtionOverDistance :: Double -> RGB -> RGB
absorbtionOverDistance trace_distance absorb_rgb = mapRGB (** trace_distance) absorb_rgb
\end{code}

\texttt{emissionOverDistance} takes a distance and the color of light being emitted by the media
through a distance of 1, and answers the resulting light being emitted through that distance.

\begin{code}
emissionOverDistance :: Double -> RGB -> RGB
emissionOverDistance trace_distance emit_rgb = scaleRGB trace_distance emit_rgb
\end{code}

\texttt{traceScattering} generates a pair (scattered light color,absorbtion filter color) by taking many samples of a
\texttt{Scattering} medium along a line segment and accumulating the total scattering and absorbtion along that
line segment.  The density and composition of the \texttt{Scattering} medium as well as the direction
and color of the light source may very with location.

Scattered light is subject to absorbtion on the way back to the eye, use \texttt{withoutAbsorbtion}
if you don't want this.  Because of this back-absorbtion, numerous samples are necessary even if the
medium and light source are constant.  The \texttt{source} and \texttt{destination} points are not commutative: the
\texttt{source} point should be position of the eye or camera or the nearest point to the eye along a ray passing
from the eye through a media object.

The second element of the result is normally the same as would be generated from traceAbsorbtion.

\begin{code}
traceScattering :: (Point3D -> Scattering) -> (Point3D -> (Vector3D,RGB)) -> SamplingAlgorithm (RGB,RGB) -> Point3D -> Point3D -> Samples (RGB,RGB)
traceScattering scatteringF lightingF samplingF source destination number_of_samples = 
    foldl' (\(summed_scattering,summed_absorbtion) (this_scattering,this_absorbtion) -> (addRGB summed_scattering (filterRGB summed_absorbtion this_scattering),
                                                                                         filterRGB summed_absorbtion this_absorbtion))
	   (gray 0,gray 1) $ sampleScattering scatteringF lightingF samplingF source destination number_of_samples

sampleScattering :: (Point3D -> Scattering) -> (Point3D -> (Vector3D,RGB)) -> SamplingAlgorithm (RGB,RGB) -> Point3D -> Point3D -> Samples [(RGB,RGB)]
sampleScattering scatteringF lightingF sampleF source destination = sampleF (\d p -> (scatteredLightAt d p,absorbedLightAt d p))
                                                                            source destination
    where vector_to_viewer = vectorToFrom source destination
          scatteredLightAt this_distance this_point = 
	      let s = scatteringF this_point
	          (light_vector,light_color) = lightingF this_point
		  scattering_angle = angleBetween vector_to_viewer light_vector
		  scatter_color = scattering_scatter s scattering_angle
	          in emissionOverDistance this_distance $ filterRGB scatter_color light_color
          absorbedLightAt this_distance this_point = absorbtionOverDistance this_distance $ scattering_absorb $ scatteringF this_point
\end{code}

\texttt{traceAbsorbtion} takes many samples of a \texttt{Scattering} medium along a line segment and accumulates
a total absorbtion along that line segment.  For a constant medium, a single sample may be adequate.

\begin{code}
traceAbsorbtion :: (Point3D -> Scattering) -> SamplingAlgorithm RGB -> Point3D -> Point3D -> Samples RGB
traceAbsorbtion scatteringF samplingF source destination number_of_samples =
    foldr filterRGB (gray 1) $ samplingF (\d p -> absorbtionOverDistance d $ scattering_absorb $ scatteringF p) source destination number_of_samples
\end{code}

\subsection{Sampling}

\texttt{linearSamples} takes samples evenly spaced along a line segment.

\begin{code}
type Samples x = Integer -> x
type SamplingAlgorithm a = (Double -> Point3D -> a) -> Point3D -> Point3D -> Samples [a]

class AdaptiveSample a where
    conspicuous :: a -> Double

instance AdaptiveSample RGB where
    conspicuous = recip . minRGB

instance AdaptiveSample (RGB,RGB) where
    conspicuous (scattering_color,absorbtion_color) = maxRGB scattering_color / minRGB absorbtion_color

data Sample a = Sample {
    sample_conspic :: Double,
    sample_value :: a,
    sample_source :: Point3D,
    sample_midpoint :: Point3D,
    sample_destination :: Point3D }

linearSamples :: SamplingAlgorithm a
linearSamples sampleF source destination number_of_samples = map (sampleF sample_distances) sample_points
    where sample_points = map (flip lerp (source,destination)) $ zeroToOne number_of_samples
          sample_distances = distanceBetween source destination / fromInteger number_of_samples

-- | 'adaptiveSamples' tries to selectively subdivide samples that seem most \"conspicuous\" using a user-supplied
-- \"conspicuous-ness\" function.  This should give a better result in less samples for highly detailed media models,
-- but is likely to be slower that 'linearSamples' for the same number of samples.
adaptiveSamples :: (AdaptiveSample a) => SamplingAlgorithm a
adaptiveSamples sampleF source destination number_of_samples = map sample_value $ head $ 
              dropWhile ((< fromInteger number_of_samples) . length) $ 
	      iterate (\samples -> concatMap (resampleRecursive 0 $ medianSamples samples) samples) seed_samples
    where seed_samples = [sampleBetween source destination]
          sampleBetween a b = Sample { sample_conspic = conspicuous s,
				       sample_value = s,
				       sample_source = a,
				       sample_midpoint = p,
			 	       sample_destination = b }
	      where p = lerp 0.5 (a,b)
	            s = sampleF (distanceBetween a b) p
	  medianSamples samples = head $ drop (length conspics `div` 2) conspics
	      where conspics = sort $ map sample_conspic samples
	  recursive_limit = max 1 $ floor $ log (realToFrac number_of_samples) / log 4
	  resampleRecursive limit _ sample | limit > recursive_limit = [sample]
	  resampleRecursive _ threshold sample | sample_conspic sample < threshold = [sample]
          resampleRecursive limit threshold sample = first_samples ++ second_samples
	      where first_samples = resampleRecursive (limit+1) threshold $ sampleBetween (sample_source sample) (sample_midpoint sample)
	            second_samples = resampleRecursive (limit+1) threshold $ sampleBetween (sample_midpoint sample) (sample_destination sample)
\end{code}

\subsection{Specific Scattering Functions}

Elastic media \"absorb\" exactly the same amount of light as they scatter.  If this light is colored then results
may be strange, since without a global illumination model scattered light doesn't continue to propigate through
the medium.  Therefore, use \texttt{achromaticAbsorbtion} when measuring scattering.

For elastic media, the absorbed color is the inverse of the scattering color.  All media take the scattering
color as their parameter.  Inelastic media scatter less light than they absorb, and it may be the same color
or achromatic.

All of these \texttt{Scattering} media models accept a distance parameter.  The greater the distance, the thinner
the media.  For example, \texttt{fog 5.0 (rgb 0.5 0.25 0.1)} absorbs 25\% of all green light passing through 5 units
of the medium.

\texttt{dust} represents many macroscopic spheres suspended in the atmosphere.
This is an inelastic medium that always features achromatic absorbtion.

\begin{code}
dust :: Double -> RGB -> Scattering
dust d c = adjustDistance d $ Scattering {
    scattering_absorb = gray 0.5,
    scattering_scatter = flip scaleRGB c . (*0.5) . (1-) . (*2) . realToFrac . toRotations_ }
\end{code}

\texttt{fog} is a colored media that scatters and absorbs the same color.  \texttt{fog}
might also be appropriate colored media for a solid translucent object.  This is an inelastic medium.
See \texttt{elasticOmnidirectionalScatter} for an elastic version of \texttt{fog}.

\begin{code}
fog :: Double -> RGB -> Scattering
fog d c = adjustDistance d $ Scattering {
    scattering_absorb = c,
    scattering_scatter = const c }
\end{code}

\texttt{rayleigh} when used with \texttt{rayleigh_sky} as the color parameter simulates elastic scattering 
of light by very fine dust or even thin air.

\begin{code}
rayleigh_sky :: RGB
rayleigh_sky = rgb 0.06 0.10 0.23 

rayleigh :: Double -> RGB -> Scattering
rayleigh d c = adjustDistance d $ Scattering {
    scattering_absorb = invertRGB c,
    scattering_scatter = \theta -> scaleRGB ((1 + realToFrac (cosine theta)^2)*0.75 ) c }
\end{code}

\texttt{elasticBackScatter} throws light back at the light source within a spcified cone.
This medium doesn't absorb much light, especially when the scattering angle is very small,
because while the scattered light may be very bright, being focused it doesn't represent
as much energy as other \texttt{Scattering} media.

\begin{code}
elasticBackScatter :: Double -> Angle -> RGB -> Scattering
elasticBackScatter d a c_ = adjustDistance d $ Scattering {
    scattering_absorb = invertRGB c,
    scattering_scatter = \theta -> scaleRGB (realToFrac $ max 0 $ n*(r - (toRadians_ theta)^2)) c }
        where r_ = toRadians a
              r = r_^2
	      n = recip $ (1 + r/2 - sin(r_)*r_ - cos(r_))
	      c = scaleRGB (realToFrac $ (toRotations a * 4)^2) c_
\end{code}

\texttt{elasticForwardScatter} is the reverse of \texttt{elasticBackScatter}.  Use this
to immitate Mie scattering.

\begin{code}
elasticForwardScatter :: Double -> Angle -> RGB -> Scattering
elasticForwardScatter d a c = s {
    scattering_scatter = scattering_scatter s . supplementaryAngle }
        where s = elasticBackScatter d a c
\end{code}

\texttt{elasticOmnidirectionScatter} is like an elastic \texttt{fog}, scattering equally in every direction.

\begin{code}
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
