\section{Pre-specified Colors, Materials and Deformations}

\begin{code}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RSAGL.ModelingExtras
    (gray,
     gray256,
     glass,
     plastic,
     metallic,
     cloudy,
     spherical,
     directional,
     gradient,
     bumps,
     waves,
     module RSAGL.RSAGLColors,
     module RSAGL.Material,
     module RSAGL.ApplicativeWrapper,
     module Control.Applicative)
    where

import Graphics.Rendering.OpenGL.GL.BasicTypes
import RSAGL.Noise
import RSAGL.RSAGLColors
import Control.Applicative
import RSAGL.ApplicativeWrapper
import RSAGL.Vector
import RSAGL.Material
import RSAGL.Affine
import RSAGL.Model
\end{code}

\subsection{Colors}

\texttt{RSAGL.ModellingSupport} exports the contents of \texttt{rsagl-colors.txt}.  This file is translated to HTML and Haskell source by the script in \texttt{ProcessColors.hs}.  Color samples can be viewed by opening the file \texttt{rsagl-colors.html} in a web browser.

With the exception of \texttt{blackbody}, all of the colors contain non-zero values for the red, green, and blue components, so that extremely bright lights will (realistically) wash those colors out to white.

\begin{code}
gray :: Float -> RGB
gray x = rgb x x x

gray256 :: (Integral i) => i -> RGB
gray256 x = rgb256 x x x
\end{code}

\subsection{Patterns}

\texttt{cloudy} is a pattern made using perlin noise.  \texttt{spherical} is a pattern that ranges from the center of a sphere to its radius, where the center maps to zero and the radius maps to one.  \texttt{directional} is a pattern based on the directional (infinite) light source.  An object rendered with an emissive layer defined by a directional light source will seem to be lit from that direction.

\begin{code}
type ColorFunction a = ApplicativeWrapper ((->) SurfaceVertex3D) a

pattern :: (ColorClass a) => (SurfaceVertex3D -> Double) -> [(GLfloat,ColorFunction a)] -> ColorFunction a
pattern _ [(_,constant_pattern)] = constant_pattern
pattern f color_map = wrapApplicative (\sv3d -> toApplicative (lerpColorMap (realToFrac $ f sv3d) color_map) $ sv3d)

cloudy :: (ColorClass a) => Double -> [(GLfloat,ColorFunction a)] -> ColorFunction a
cloudy wave_length = pattern (\(SurfaceVertex3D p _ _) -> perlinNoise (scale' frequency p) * 0.5 + 0.5)
    where frequency = recip wave_length

spherical :: (ColorClass a) => Point3D -> Double -> [(GLfloat,ColorFunction a)] -> ColorFunction a
spherical center radius = pattern (\(SurfaceVertex3D p _ _) -> distanceBetween center p / radius)

directional :: (ColorClass a) => Vector3D -> [(GLfloat,ColorFunction a)] -> ColorFunction a
directional vector = pattern (\(SurfaceVertex3D _ v _) -> dotProduct (vectorNormalize v) (vectorNormalize vector))

gradient :: (ColorClass a) => Point3D -> Vector3D -> [(GLfloat,ColorFunction a)] -> ColorFunction a
gradient center vector = pattern
    (\(SurfaceVertex3D p _ _) -> dotProduct vector (vectorToFrom p center) * l)
        where l = recip (vectorLength vector) ^ 2

lerpColorMap :: (ColorClass a) => GLfloat -> [(GLfloat,ColorFunction a)] -> ColorFunction a
lerpColorMap _ [] = error "lerpColorMap: empty color map"
lerpColorMap _ [(_,f)] = f
lerpColorMap u ((x,f):_) | x >= u = f
lerpColorMap u ((x,f):(y,g):_) | u > x && u < y = (curry $ lerpColor ((u - x) / (y - x))) <$> f <*> g
lerpColorMap u rest = lerpColorMap u (tail rest)
\end{code}

\subsection{Materials}

\begin{code}
glass :: RGBFunction -> Modeling attr
glass rgbf = 
    do transparent $ (alpha 0.05) <$> rgbf
       specular 100 $ (\rgb_color -> curry (lerpColor (brightness rgb_color)) rgb_color white) <$> rgbf

plastic :: RGBFunction -> Modeling attr
plastic rgbf = 
    do pigment rgbf
       specular 50 (pure white)

metallic :: RGBFunction -> Modeling attr
metallic rgbf = 
    do pigment rgbf
       specular 75 rgbf
\end{code}

\subsection{Deformations}

\texttt{bumps} can be used to describe any deformation in which vertices are purturbed in the direction of their normal vectors.

\texttt{waves} is a deformation that makes little waves in a surface.

\begin{code}
bumps :: (Point3D -> Double) -> Modeling attr
bumps f = deform $ (\(p,v) -> translate (vectorScale (f p) v) p)

waves :: Double -> Double -> Modeling attr
waves wave_length amplitude = bumps (\(Point3D x y z) -> sin ((x+y+z) / wave_length * 2*pi) * amplitude)
\end{code}
