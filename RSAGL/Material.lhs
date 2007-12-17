\section{Materials}

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Material
    (module RSAGL.Color,
     MaterialLayer,MaterialSurface,Material,
     toLayers,materialLayerSurface,materialLayerRelevant,materialComplexity,materialLayerToOpenGLWrapper,
     diffuseLayer,RSAGL.Material.specularLayer,transparentLayer,emissiveLayer)
    where

import Data.Maybe
import Data.Monoid
import Control.Applicative
import RSAGL.Color
import RSAGL.ApplicativeWrapper
import RSAGL.Surface
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL.GL.Colors
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.PerFragment
\end{code}

\subsection{MaterialLayers}

A \texttt{MaterialLayer} is a layer of material some material quality (diffuse, transparent, emissive, or specular highlight).  MaterialLayers are rendered one on top of another to create layered effects.

\begin{code}
type MaterialSurface = ApplicativeWrapper Surface

data MaterialLayer =
    DiffuseLayer (MaterialSurface RGB)
  | TransparentLayer (MaterialSurface RGBA)
  | EmissiveLayer (MaterialSurface RGB)
  | SpecularLayer (MaterialSurface RGB) GLfloat
  | CompoundLayer (MaterialSurface RGB) RGB RGB GLfloat

instance NFData MaterialLayer where
    rnf (DiffuseLayer msrgb) = rnf msrgb
    rnf (TransparentLayer msrgba) = rnf msrgba
    rnf (EmissiveLayer msrgb) = rnf msrgb
    rnf (SpecularLayer msrgb shininess) = rnf (msrgb,shininess)
    rnf (CompoundLayer msrgb spec emis shininess) = rnf (msrgb,spec,emis,shininess)

data Material = Material [MaterialLayer]

toLayers :: Material -> [MaterialLayer]
toLayers (Material layers) = layers

combineLayers :: [MaterialLayer] -> [MaterialLayer]
combineLayers (x1:x2:xs) | isJust (combine2Layers x1 x2) = combineLayers $ fromJust (combine2Layers x1 x2) : xs
combineLayers (x:xs) = x : combineLayers xs
combineLayers xs = xs

combine2Layers :: MaterialLayer -> MaterialLayer -> Maybe MaterialLayer
combine2Layers (DiffuseLayer msrgb) (SpecularLayer specular_rgb shininess) | isPure specular_rgb =
    Just $ CompoundLayer msrgb (fromJust $ fromPure $ specular_rgb) (RGB 0 0 0) shininess
combine2Layers (DiffuseLayer msrgb) (EmissiveLayer emissive_rgb) | isPure emissive_rgb =
    Just $ CompoundLayer msrgb (RGB 0 0 0) (fromJust $ fromPure $ emissive_rgb) 0
combine2Layers (EmissiveLayer x) (EmissiveLayer y) = Just $ EmissiveLayer $ addRGB <$> x <*> y
combine2Layers (CompoundLayer msrgb specular_rgb emissive_rgb1 shininess) (EmissiveLayer emissive_rgb2) | isPure emissive_rgb2 =
    Just $ CompoundLayer msrgb specular_rgb (addRGB emissive_rgb1 (fromJust $ fromPure $ emissive_rgb2)) shininess
combine2Layers (CompoundLayer msrgb (RGB 0 0 0) emissive_rgb 0) (SpecularLayer specular_rgb shininess) | isPure specular_rgb =
    Just $ CompoundLayer msrgb (fromJust $ fromPure $ specular_rgb) emissive_rgb shininess
combine2Layers _ _ = Nothing

instance Monoid Material where
    mempty = Material []
    mappend (Material xs) (Material ys) = Material $ combineLayers $ (\zs -> if null (snd zs) then fst zs else snd zs) $
                                                     span (not . isOpaqueLayer) $ xs ++ ys

materialLayerComplexity :: MaterialLayer -> Integer
materialLayerComplexity layer | fromPure (materialLayerRelevant layer) == Just False = 0
materialLayerComplexity (DiffuseLayer ms) | fromPure ms == Just (RGB 0 0 0) = 0
materialLayerComplexity (DiffuseLayer ms) | isPure ms = 1
materialLayerComplexity (DiffuseLayer {}) = 2
materialLayerComplexity (TransparentLayer ms) | isPure ms = 1
materialLayerComplexity (TransparentLayer {}) = 2
materialLayerComplexity (EmissiveLayer ms) | isPure ms = 0
materialLayerComplexity (EmissiveLayer {}) = 2
materialLayerComplexity (SpecularLayer ms _) | isPure ms = 3
materialLayerComplexity (SpecularLayer {}) = 4
materialLayerComplexity (CompoundLayer msrgb specular_rgb emissive_rgb shininess) = 
    sum $ map materialLayerComplexity [DiffuseLayer msrgb,SpecularLayer (pure specular_rgb) shininess,EmissiveLayer (pure emissive_rgb)]

materialComplexity :: Material -> Integer
materialComplexity (Material layers) = sum $ map materialLayerComplexity layers

isOpaqueLayer :: MaterialLayer -> Bool
isOpaqueLayer (DiffuseLayer _) = True
isOpaqueLayer (TransparentLayer ms) | fmap rgba_a (fromPure ms) == Just 1.0 = True
isOpaqueLayer (EmissiveLayer ms) | fromPure ms == Just (RGB 1.0 1.0 1.0) = True
isOpaqueLayer (CompoundLayer _ _ _ _) = True
isOpaqueLayer _ = False

isEmissiveRelevant :: RGB -> Bool
isEmissiveRelevant (RGB 0 0 0) = False
isEmissiveRelevant _ = True

isTransparentRelevant :: RGBA -> Bool
isTransparentRelevant (RGBA 0 _) = False
isTransparentRelevant _ = True

materialLayerSurface :: MaterialLayer -> MaterialSurface RGBA
materialLayerSurface (DiffuseLayer msrgb) = fmap toRGBA msrgb
materialLayerSurface (TransparentLayer msrgba) = msrgba
materialLayerSurface (EmissiveLayer msrgb) = fmap toRGBA msrgb
materialLayerSurface (SpecularLayer msrgb _) = fmap toRGBA msrgb
materialLayerSurface (CompoundLayer msrgb _ _ _) = fmap toRGBA msrgb

materialLayerRelevant :: MaterialLayer -> MaterialSurface Bool
materialLayerRelevant (DiffuseLayer {}) = pure True
materialLayerRelevant (TransparentLayer msrgba) = fmap isTransparentRelevant msrgba
materialLayerRelevant (EmissiveLayer msrgb) = fmap isEmissiveRelevant msrgb
materialLayerRelevant (SpecularLayer msrgb _) = fmap isEmissiveRelevant msrgb
materialLayerRelevant (CompoundLayer {}) = pure True

materialLayerToOpenGLWrapper :: MaterialLayer -> IO () -> IO ()
materialLayerToOpenGLWrapper (DiffuseLayer ms) io = 
    do cm <- get colorMaterial
       materialEmission FrontAndBack $= Color4 0 0 0 1
       materialSpecular FrontAndBack $= Color4 0 0 0 1
       colorMaterial $= Just (FrontAndBack,AmbientAndDiffuse)
       maybe (return ()) (rgbToOpenGL) $ fromPure ms
       io
       colorMaterial $= cm
materialLayerToOpenGLWrapper (TransparentLayer ms) io = 
    do cm <- get colorMaterial
       materialEmission FrontAndBack $= Color4 0 0 0 1
       materialSpecular FrontAndBack $= Color4 0 0 0 1
       colorMaterial $= Just (FrontAndBack,AmbientAndDiffuse)
       maybe (return ()) (rgbaToOpenGL) $ fromPure ms
       alphaBlendWrapper io
       colorMaterial $= cm
materialLayerToOpenGLWrapper (EmissiveLayer ms) io = 
    do l <- get lighting
       lighting $= Disabled
       maybe (return ()) (rgbToOpenGL) $ fromPure ms
       additiveBlendWrapper io
       lighting $= l
materialLayerToOpenGLWrapper (SpecularLayer ms shininess) io = 
    do cm <- get colorMaterial
       lmlv <- get lightModelLocalViewer
       materialShininess FrontAndBack $= shininess
       materialAmbientAndDiffuse FrontAndBack $= Color4 0 0 0 1
       materialEmission FrontAndBack $= Color4 0 0 0 1
       colorMaterial $= Just (FrontAndBack,Specular)
       lightModelLocalViewer $= Enabled
       maybe (return ()) (rgbToOpenGL) $ fromPure ms
       additiveBlendWrapper io
       colorMaterial $= cm
       lightModelLocalViewer $= lmlv
materialLayerToOpenGLWrapper (CompoundLayer ms specular_rgb emissive_rgb shininess) io =
    do cm <- get colorMaterial
       lmlv <- get lightModelLocalViewer
       materialSpecular FrontAndBack $= (\(RGB r g b) -> Color4 r g b 1) specular_rgb
       materialShininess FrontAndBack $= shininess
       materialEmission FrontAndBack $= (\(RGB r g b) -> Color4 r g b 1) emissive_rgb
       colorMaterial $= Just (FrontAndBack,AmbientAndDiffuse)
       lightModelLocalViewer $= Enabled
       maybe (return ()) (rgbToOpenGL) $ fromPure ms
       io
       colorMaterial $= cm
       lightModelLocalViewer $= lmlv

alphaBlendWrapper :: IO () -> IO ()
alphaBlendWrapper io =
    do bf <- get blendFunc
       b <- get blend
       blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
       blend $= Enabled
       io
       blendFunc $= bf
       blend $= b

additiveBlendWrapper :: IO () -> IO ()
additiveBlendWrapper io =
    do bf <- get blendFunc
       b <- get blend
       blendFunc $= (One,One)
       blend $= Enabled
       io
       blendFunc $= bf
       blend $= b
\end{code}

\subsection{Constructing Materials}

\begin{code}
diffuseLayer :: MaterialSurface RGB -> Material
diffuseLayer msrgb = Material [DiffuseLayer msrgb]

specularLayer :: MaterialSurface RGB -> GLfloat -> Material
specularLayer msrgb x = Material [SpecularLayer msrgb x]

transparentLayer :: MaterialSurface RGBA -> Material
transparentLayer msrgba = Material [TransparentLayer msrgba]

emissiveLayer :: MaterialSurface RGB -> Material
emissiveLayer msrgb = Material [EmissiveLayer msrgb]
\end{code}
