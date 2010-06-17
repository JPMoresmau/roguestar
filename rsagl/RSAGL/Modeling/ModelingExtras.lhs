\section{Pre-specified Colors, Models, Materials and Deformations}

\begin{code}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RSAGL.Modeling.ModelingExtras
    (smoothbox,
     regularPrism,
     heightField,
     heightDisc,
     rotationGroup,
     glass,
     plastic,
     metallic,
     pattern,
     cloudy,
     blinkBoxes,
     spherical,
     directional,
     gradient,
     bumps,
     waves,
     heightMap,
     disregardSurfaceNormals,
     ColorFunction,
     Pattern,
     dropRandomElements,
     module RSAGL.Modeling.RSAGLColors,
     module RSAGL.Modeling.Material,
     module RSAGL.Auxiliary.ApplicativeWrapper,
     module Control.Applicative)
    where

import Graphics.Rendering.OpenGL.GL hiding (translate,rotate,scale,specular)
import RSAGL.Modeling.Noise
import RSAGL.Modeling.RSAGLColors
import Control.Applicative
import RSAGL.Auxiliary.ApplicativeWrapper
import RSAGL.Math.Vector
import RSAGL.Modeling.Material
import RSAGL.Math.Affine
import RSAGL.Modeling.Model
import System.Random
import RSAGL.Math.Interpolation
import Data.Monoid
import RSAGL.Auxiliary.Auxiliary
import RSAGL.Math.Angle
import RSAGL.Math.Ray
import RSAGL.Types
\end{code}

\subsection{Colors}

\texttt{RSAGL.ModellingSupport} exports the contents of \texttt{rsagl-colors.txt}.  This file is translated to HTML and Haskell source by the script in \texttt{ProcessColors.hs}.  Color samples can be viewed by opening the file \texttt{rsagl-colors.html} in a web browser.

With the exception of \texttt{blackbody}, all of the colors contain non-zero values for the red, green, and blue components, so that extremely bright lights will (realistically) wash those colors out to white.

\subsection{Models}

\texttt{smoothbox} is a box that takes an extra smoothing parameter between 0 and 1.  This box doesn't have perfectly flat normals, and may therefore be a little easier on the eye.

\begin{code}
smoothbox :: (Monoid attr) => RSdouble -> Point3D -> Point3D -> Modeling attr
smoothbox u p q = model $
    do box p q
       deform $ \(SurfaceVertex3D point vector) -> SurfaceVertex3D point $ vectorNormalize $ lerp u (vector,vectorNormalize $ vectorToFrom point midpoint)
        where midpoint = lerp 0.5 (p,q)
\end{code}

\texttt{regularPrism} constructs a regular n-sided prism or pyramid.

\begin{code}
regularPrism ::(Monoid attr) => (Point3D,RSdouble) -> (Point3D,RSdouble) -> Integer -> Modeling attr
regularPrism (a,ra) (b,rb) n = 
    model $ translate (vectorToFrom a origin_point_3d) $ 
        rotateToFrom (Vector3D 0 1 0) (vectorToFrom b a) $ sequence_ $ rotationGroup (Vector3D 0 1 0) n $ quad
  where a1 = Point3D 0 0 ra
        a2 = rotateY (fromRotations $ recip $ fromInteger n) a1
        b1 = Point3D 0 (distanceBetween a b) rb
        b2 = rotateY (fromRotations $ recip $ fromInteger n) b1
        quad = quadralateral a1 a2 b2 b1
\end{code}

\begin{code}
-- | A rectangular height field rising off of the x-z plane.
heightField :: (Monoid attr) => (RSdouble,RSdouble) -> (RSdouble,RSdouble) -> ((RSdouble,RSdouble) -> RSdouble) -> Modeling attr
heightField (x1,z1) (x2,z2) f = model $
    do quadralateral (Point3D x1 0 z1) (Point3D x1 0 z2) (Point3D x2 0 z2) (Point3D x2 0 z1)
       heightMap f
       
\end{code}

\begin{code}
-- | A circular height field rising off of the x-z plane.
heightDisc :: (Monoid attr) => (RSdouble,RSdouble) -> RSdouble -> ((RSdouble,RSdouble) -> RSdouble) -> Modeling attr
heightDisc (x,y) r f = model $
    do closedDisc (Point3D x 0 y) (Vector3D 0 1 0) r
       heightMap f
\end{code}

\texttt{rotationGroup} rotates a model repeatedly.

\begin{code}
rotationGroup :: (AffineTransformable a) => Vector3D -> Integer -> a -> [a]
rotationGroup v n m = map (flip (rotate v) m . fromRotations) $ tail $ zeroToOne (n+1)
\end{code}

\subsection{Patterns}

\texttt{cloudy} is a pattern made using perlin noise.  \texttt{spherical} is a pattern that ranges from the center of a sphere to its radius, where the center maps to zero and the radius maps to one.  \texttt{directional} is a pattern based on the directional (infinite) light source.  An object rendered with an emissive layer defined by a directional light source will seem to be lit from that direction.

\begin{code}
type ColorFunction a = ApplicativeWrapper ((->) SurfaceVertex3D) a

type Pattern = SurfaceVertex3D -> RSdouble

pattern :: (ColorClass a) => Pattern -> [(GLfloat,ColorFunction a)] -> ColorFunction a
pattern _ [(_,constant_pattern)] = constant_pattern
pattern f color_map = wrapApplicative (\sv3d -> toApplicative (lerpMap color_map $ f2f $ f sv3d) $ sv3d)

cloudy :: Int -> RSdouble -> Pattern
cloudy seed wave_length (SurfaceVertex3D p _) = perlinNoise (translate offset $ scale' frequency p) + 0.5
    where frequency = recip wave_length
          offset = vectorNormalize $ fst $ randomXYZ (-1000.0*wave_length,1000.0*wave_length) (mkStdGen seed)

blinkBoxes :: Int -> RSdouble -> RSdouble -> RSdouble -> Pattern
blinkBoxes seed box_size chaos threshold = thresholdF . cloudy seed (recip chaos) . toLatticeCoordinates
    where thresholdF u = if u > threshold then 1.0 else 0.0
          toLatticeCoordinates (SurfaceVertex3D (Point3D x y z) v) = 
              SurfaceVertex3D (Point3D (to1LatticeCoordinate x) (to1LatticeCoordinate y) (to1LatticeCoordinate z)) v
          to1LatticeCoordinate u = fromInteger $ round $ u/box_size

spherical :: Point3D -> RSdouble -> Pattern
spherical center radius (SurfaceVertex3D p _) = distanceBetween center p / radius

directional :: Vector3D -> Pattern
directional vector (SurfaceVertex3D _ v) = dotProduct (vectorNormalize v) normalized_vector
    where normalized_vector = vectorNormalize vector

gradient :: Point3D -> Vector3D -> Pattern
gradient center vector (SurfaceVertex3D p _) = distanceAlong (Ray3D center vector) p
\end{code}

\subsection{Materials}

\begin{code}
glass :: RGBFunction -> MaterialM attr ()
glass rgbf = 
    do transparent $ (alpha 0.05) <$> rgbf
       specular 100 $ (\rgb_color -> curry (lerp (brightness rgb_color)) rgb_color white) <$> rgbf

plastic :: RGBFunction -> MaterialM attr ()
plastic rgbf = 
    do pigment rgbf
       specular 50 (pure white)

metallic :: RGBFunction -> MaterialM attr ()
metallic rgbf = 
    do pigment rgbf
       specular 75 rgbf
\end{code}

\subsection{Deformations}

\texttt{bumps} can be used to describe any deformation in which vertices are purturbed in the direction of their normal vectors.

\texttt{waves} is a deformation that makes little waves in a surface.

\begin{code}
bumps :: Pattern -> Modeling attr
bumps f = deform $ \(sv3d@(SurfaceVertex3D p v)) -> translate (vectorScale (f sv3d) v) p

waves :: RSdouble -> RSdouble -> Pattern
waves wave_length amplitude (SurfaceVertex3D (Point3D x y z) _) = (wave_f x + wave_f y + wave_f z) * amplitude / 3
    where wave_f u = sin (u / wave_length * 2*pi)

-- | Raises or lowers each point in a model along the y-axis according to its (x,z) coordinate.
-- Typically this is used to construct height fields.
--
heightMap :: ((RSdouble,RSdouble) -> RSdouble) -> Modeling attr
heightMap f = deform $ \(Point3D x y z) -> Point3D x (y + f (x,z)) z

-- | For models where we are certain surface normals don't matter, then don't calculate them.
disregardSurfaceNormals :: Modeling attr
disregardSurfaceNormals = deform $ \(SurfaceVertex3D p _) -> SurfaceVertex3D p (Vector3D 0 1 0)
\end{code}
