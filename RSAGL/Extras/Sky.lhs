\section{Sky}

Generates a realistic sky.

\begin{code}
module RSAGL.Extras.Sky
    (sky)
    where

import RSAGL.Scattering
import RSAGL.Ray
import RSAGL.RayTrace
import RSAGL.Vector
import RSAGL.Color
import RSAGL.Angle
\end{code}

\begin{code}
sky :: Double -> Double -> Double -> (Point3D -> Scattering) -> (Vector3D,RGB) -> Vector3D -> Integer -> RGB
sky planet_radius sky_radius camera_altitude scatteringF (sun_vector_,sun_color) look_vector_ number_of_samples = scattering
    where sun_vector = vectorNormalize sun_vector_
          look_vector = vectorNormalize look_vector_
          camera_p = Point3D 0 camera_altitude 0
          lightingF p = case (testRay1st (Ray3D p sun_vector) planet_sphere,
	                      testRay1st (Ray3D p sun_vector) sky_sphere) of
			     (Just _,_) -> (sun_vector,gray 0)
			     (Nothing,Just (_,SurfaceVertex3D sky_p sky_v)) | angleBetween sky_v sun_vector < (fromDegrees 90) -> 
			         (sun_vector,filterRGB (traceAbsorbtion scatteringF p sky_p number_of_samples) sun_color) 
			     _ -> (sun_vector,sun_color)
	  sky_sphere = sphere (origin_point_3d) sky_radius
	  planet_sphere = sphere (origin_point_3d) planet_radius
	  scattering = case testRay1st (Ray3D camera_p look_vector) sky_sphere of
			   (Just (_,SurfaceVertex3D sky_p sky_v)) | angleBetween sky_v look_vector_ < (fromDegrees 90) ->
			       traceScattering scatteringF lightingF camera_p sky_p number_of_samples
	                   _ -> gray 0
\end{code}
