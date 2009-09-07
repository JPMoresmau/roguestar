{-# OPTIONS_GHC -fglasgow-exts #-}

-- | RSAGL.Material handles properties of surfaces such as color, shininess, and transparency
-- including procedural textures but not including anything touching the normal vector, such
-- as bumpiness.  Materials are handled using layers.
--
module RSAGL.Modeling.Material
    (module RSAGL.Modeling.Color,
     MaterialLayer,MaterialSurface,Material,materialIsEmpty,
     toLayers,materialLayerSurface,materialLayerRelevant,materialComplexity,materialLayerToOpenGLWrapper,
     isOpaqueLayer,
     diffuseLayer,RSAGL.Modeling.Material.specularLayer,transparentLayer,emissiveLayer,filteringLayer)
    where

import Data.Maybe
import Data.Monoid
import Control.Applicative
import RSAGL.Modeling.Color
import RSAGL.Math.Curve
import RSAGL.Auxiliary.ApplicativeWrapper
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL.GL hiding (RGB,RGBA)

-- | A 'MaterialSurface' is parameterized either on RGB or RGBA, depending
-- on whether or not the 'MaterialLayer' is capable of transparency.
-- 'MaterialSurface's that are only one color (as opposed to procedural textures)
-- can be described using 'pure', for example "pure red".
--
type MaterialSurface a = ApplicativeWrapper Surface a

-- | A 'MaterialLayer' is a layer of material some material quality (diffuse, transparent, emissive, or specular highlight).  
-- 'MaterialLayers' are rendered one on top of another to create layered effects.
data MaterialLayer =
    -- | A simple colored material.
    DiffuseLayer (MaterialSurface RGB)
    -- | A transparent colored material.
  | TransparentLayer (MaterialSurface RGBA)
    -- | A glowing material.
  | EmissiveLayer (MaterialSurface RGB)
    -- | A shiny material with specular highlight.  Includes the specular exponent.
  | SpecularLayer (MaterialSurface RGB) GLfloat
    -- | A compound layer of diffuse, pure specular, and pure emissive layers.  This is a common use case, and therefore optimized into one layer.
  | CompoundLayer (MaterialSurface RGB) RGB RGB GLfloat
    -- | A layer that filters (multiplies) light from behind, but doesn't reflect or glow at all.
  | FilterLayer (MaterialSurface RGB)

instance NFData MaterialLayer where
    rnf (DiffuseLayer msrgb) = rnf msrgb
    rnf (TransparentLayer msrgba) = rnf msrgba
    rnf (EmissiveLayer msrgb) = rnf msrgb
    rnf (SpecularLayer msrgb shininess) = shininess `seq` rnf msrgb
    rnf (CompoundLayer msrgb spec emis shininess) = shininess `seq` rnf (msrgb,spec,emis)
    rnf (FilterLayer msrgb) = rnf msrgb

-- | A stack of 'MaterialLayer's.  'Material' is smart about compressing multiple layers into the least of number of equivalent layers.
data Material = Material [MaterialLayer]

-- | Split open a Material into its component layers.
toLayers :: Material -> [MaterialLayer]
toLayers (Material layers) = layers

-- | Wherever possible, combine material layers into one material layer.
-- For example, two emissive layers can be directly added together.
-- See 'combine2Layers'.
combineLayers :: [MaterialLayer] -> [MaterialLayer]
combineLayers (x1:x2:xs) | Just x <- combine2Layers x1 x2 = 
    let combine_here = combineLayers $ x : xs
        combine_rest = combineLayers $ x2 : xs
	in case length combine_rest < length combine_here of
              True  -> combineLayers $ x1 : combine_rest
	      False -> combine_here
combineLayers (x:xs) = x : combineLayers xs
combineLayers xs = xs

-- | Tries to combine exactly two layers, or answers Nothing if the layers can't be combined.
combine2Layers :: MaterialLayer -> MaterialLayer -> Maybe MaterialLayer
-- diffuse + pure specular
combine2Layers (DiffuseLayer msrgb) (SpecularLayer specular_rgb shininess) | isPure specular_rgb =
    Just $ CompoundLayer msrgb (fromJust $ fromPure $ specular_rgb) (RGB 0 0 0) shininess
-- diffuse + pure emissive
combine2Layers (DiffuseLayer msrgb) (EmissiveLayer emissive_rgb) | isPure emissive_rgb =
    Just $ CompoundLayer msrgb (RGB 0 0 0) (fromJust $ fromPure $ emissive_rgb) 0
-- emissive + emissive
combine2Layers (EmissiveLayer x) (EmissiveLayer y) = Just $ EmissiveLayer $ addRGB <$> x <*> y
-- compound + pure emissive
combine2Layers (CompoundLayer msrgb specular_rgb emissive_rgb1 shininess) (EmissiveLayer emissive_rgb2) | isPure emissive_rgb2 =
    Just $ CompoundLayer msrgb specular_rgb (addRGB emissive_rgb1 (fromJust $ fromPure $ emissive_rgb2)) shininess
-- compound + pure specular
combine2Layers (CompoundLayer msrgb (RGB 0 0 0) emissive_rgb 0) (SpecularLayer specular_rgb shininess) | isPure specular_rgb =
    Just $ CompoundLayer msrgb (fromJust $ fromPure $ specular_rgb) emissive_rgb shininess
-- filter + filter
combine2Layers (FilterLayer x) (FilterLayer y) =
    Just $ FilterLayer $ filterRGB <$> x <*> y
combine2Layers _ _ = Nothing

instance Monoid Material where
    mempty = Material []
    mappend (Material xs) (Material ys) = Material $ combineLayers $ (\zs -> if null (snd zs) then fst zs else snd zs) $
                                                     span (not . isOpaqueLayer) $ xs ++ ys

materialIsEmpty :: Material -> Bool
materialIsEmpty (Material xs) = null xs

-- | A measure of how much color variation should be expected between vertices of a model rendered with this material.
-- Materials using procedural textures are weighted more heavily than others, and specular textures are weighted very
-- heavily.  Materials with constant properties, such as pure emissive and black diffuse layers, have a complexity of zero.
-- This is a heuristic used to assign more vertices to more complex materials.
-- 
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
materialLayerComplexity (CompoundLayer {}) = 3
materialLayerComplexity (FilterLayer ms) | isPure ms = 0
materialLayerComplexity (FilterLayer {}) = 2

-- | Answers a complexity heuristic for a 'Material'.  Result is a small integer greater than or equal to zero.
materialComplexity :: Material -> Integer
materialComplexity (Material []) = 0
materialComplexity (Material layers) = maximum $ map materialLayerComplexity layers

-- | True if the 'MaterialLayer' is completely opaque.  A layer under an opaque layer is not visible.
isOpaqueLayer :: MaterialLayer -> Bool
isOpaqueLayer (DiffuseLayer _) = True
isOpaqueLayer (TransparentLayer ms) | fmap rgba_a (fromPure ms) == Just 1.0 = True
isOpaqueLayer (CompoundLayer _ _ _ _) = True
isOpaqueLayer _ = False

-- | True if the color is not black.  Black emissive materials contribue nothing to the color of a model,
-- and can therefore be eleminated from a model.
--
isEmissiveRelevant :: RGB -> Bool
isEmissiveRelevant (RGB r g b) | r <= 0 && g <= 0 && b <= 0 = False
isEmissiveRelevant _ = True

-- | True is the color is not white.  White filter materials don't filter any light, and can therefore be
-- eleminated from a model.
isFilterRelevant :: RGB -> Bool
isFilterRelevant (RGB r g b) | r >= 1 && g >= 1 && b >= 1 = False
isFilterRelevant _ = True

-- | True if the color is not perfectly transparent.  Perfectly transparent materials are invisible, and can therefore
-- be eleminated from a model.
isTransparentRelevant :: RGBA -> Bool
isTransparentRelevant (RGBA 0 _) = False
isTransparentRelevant _ = True

-- | Get the color information for a 'MaterialLayer'.
materialLayerSurface :: MaterialLayer -> MaterialSurface RGBA
materialLayerSurface (DiffuseLayer msrgb) = fmap toRGBA msrgb
materialLayerSurface (TransparentLayer msrgba) = msrgba
materialLayerSurface (EmissiveLayer msrgb) = fmap toRGBA msrgb
materialLayerSurface (SpecularLayer msrgb _) = fmap toRGBA msrgb
materialLayerSurface (CompoundLayer msrgb _ _ _) = fmap toRGBA msrgb
materialLayerSurface (FilterLayer msrgb) = fmap toRGBA msrgb

-- | Get a relevance layer for a surface.  Purely irrelevant materials can be removed without changing the
-- appearance of a model.  Irrelevant triangles can also be selectively culled from a model.
materialLayerRelevant :: MaterialLayer -> MaterialSurface Bool
materialLayerRelevant (DiffuseLayer {}) = pure True
materialLayerRelevant (TransparentLayer msrgba) = fmap isTransparentRelevant msrgba
materialLayerRelevant (EmissiveLayer msrgb) = fmap isEmissiveRelevant msrgb
materialLayerRelevant (SpecularLayer msrgb _) = fmap isEmissiveRelevant msrgb
materialLayerRelevant (CompoundLayer {}) = pure True
materialLayerRelevant (FilterLayer msrgb) = fmap isFilterRelevant msrgb

-- | Run an IO action wrapped in OpenGL state appropriate for the layer in question.
materialLayerToOpenGLWrapper :: MaterialLayer -> IO () -> IO ()
materialLayerToOpenGLWrapper (DiffuseLayer ms) io = 
    do cm <- get colorMaterial
       materialEmission FrontAndBack $= Color4 0 0 0 1
       materialSpecular FrontAndBack $= Color4 0 0 0 1
       colorMaterial $= Just (FrontAndBack,AmbientAndDiffuse)
       maybe (return ()) (color . rgbToOpenGL) $ fromPure ms
       io
       colorMaterial $= cm
materialLayerToOpenGLWrapper (TransparentLayer ms) io = 
    do cm <- get colorMaterial
       materialEmission FrontAndBack $= Color4 0 0 0 1
       materialSpecular FrontAndBack $= Color4 0 0 0 1
       colorMaterial $= Just (FrontAndBack,AmbientAndDiffuse)
       maybe (return ()) (color . rgbaToOpenGL) $ fromPure ms
       alphaBlendWrapper io
       colorMaterial $= cm
materialLayerToOpenGLWrapper (EmissiveLayer ms) io = 
    do l <- get lighting
       lighting $= Disabled
       maybe (return ()) (color . rgbToOpenGL) $ fromPure ms
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
       maybe (return ()) (color . rgbToOpenGL) $ fromPure ms
       additiveBlendWrapper io
       colorMaterial $= cm
       lightModelLocalViewer $= lmlv
materialLayerToOpenGLWrapper (CompoundLayer ms specular_rgb emissive_rgb shininess) io =
    do cm <- get colorMaterial
       lmlv <- get lightModelLocalViewer
       materialSpecular FrontAndBack $= (\(RGB r g b) -> Color4 (realToFrac r) (realToFrac g) (realToFrac b) 1) specular_rgb
       materialShininess FrontAndBack $= shininess
       materialEmission FrontAndBack $= (\(RGB r g b) -> Color4 (realToFrac r) (realToFrac g) (realToFrac b) 1) emissive_rgb
       colorMaterial $= Just (FrontAndBack,AmbientAndDiffuse)
       lightModelLocalViewer $= Enabled
       maybe (return ()) (color . rgbToOpenGL) $ fromPure ms
       io
       colorMaterial $= cm
       lightModelLocalViewer $= lmlv
materialLayerToOpenGLWrapper (FilterLayer ms) io =
    do l <- get lighting
       lighting $= Disabled
       maybe (return ()) (color . rgbToOpenGL) $ fromPure ms
       filterBlendWrapper io
       lighting $= l

-- | Run an IO action with OpenGL blending state.  Used for transparent surfaces.
alphaBlendWrapper :: IO () -> IO ()
alphaBlendWrapper io =
    do bf <- get blendFunc
       b <- get blend
       blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
       blend $= Enabled
       io
       blendFunc $= bf
       blend $= b

-- | Run an IO action with additive blending OpenGL state.  Used for emissive surfaces.
additiveBlendWrapper :: IO () -> IO ()
additiveBlendWrapper io =
    do bf <- get blendFunc
       b <- get blend
       blendFunc $= (One,One)
       blend $= Enabled
       io
       blendFunc $= bf
       blend $= b

-- | Rune an IO action with multiplicative blending OpenGL state.  Used for filter surfaces.
filterBlendWrapper :: IO () -> IO ()
filterBlendWrapper io =
    do bf <- get blendFunc
       b <- get blend
       blendFunc $= (DstColor,Zero)
       blend $= Enabled
       io
       blendFunc $= bf
       blend $= b

-- | A simple colored material.
diffuseLayer :: MaterialSurface RGB -> Material
diffuseLayer msrgb = Material [DiffuseLayer msrgb]

-- | A shiny material with specular highlight, including a specular exponent parameter.
-- Larger exponents give tighter specular highlights, but should be less than 128 (larger than
-- that wouldn't have very much effect anyway).  Typical values are 1-10 or so.
-- 
specularLayer :: MaterialSurface RGB -> GLfloat -> Material
specularLayer msrgb x = Material [SpecularLayer msrgb x]

-- | A transparent colored material.
transparentLayer :: MaterialSurface RGBA -> Material
transparentLayer msrgba = Material [TransparentLayer msrgba]

-- | A material that seems to glow.
emissiveLayer :: MaterialSurface RGB -> Material
emissiveLayer msrgb = Material [EmissiveLayer msrgb]

-- | A material that doesn't reflect or emit life, but simply performs a multiplicative filter on whatever is behind it.
filteringLayer :: MaterialSurface RGB -> Material
filteringLayer msrgb = Material [FilterLayer msrgb]
