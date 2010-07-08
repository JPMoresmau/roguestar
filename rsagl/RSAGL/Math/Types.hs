{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RSAGL.Math.Types
    (RSfloat,
     RSdouble,
     toGLfloat,
     toGLdouble,
     fromGLfloat,
     fromGLdouble,
     f2f)
    where

import Graphics.Rendering.OpenGL.Raw.Core31
import Data.Vec as Vec
import Data.Vec.OpenGLRaw
import System.Random
import Control.Arrow

newtype RSfloat = RSfloat GLfloat
    deriving (Enum,
              Eq,
              Floating,
              Fractional,
              Num,
              Ord,
              Read,
              Real,
              RealFloat,
              RealFrac,
              Show,
              NearZero)
newtype RSdouble = RSdouble GLdouble
    deriving (Enum,
              Eq,
              Floating,
              Fractional,
              Num,
              Ord,
              Read,
              Real,
              RealFloat,
              RealFrac,
              Show,
              NearZero)

toGLfloat :: RSfloat -> GLfloat
toGLfloat (RSfloat x) = x

toGLdouble :: RSdouble -> GLdouble
toGLdouble (RSdouble x) = x

fromGLfloat :: GLfloat -> RSfloat
fromGLfloat = RSfloat

fromGLdouble :: GLdouble -> RSdouble
fromGLdouble = RSdouble

{-# RULES
"f2f/id"    f2f = id #-}
f2f :: (RealFloat a,RealFloat b) => a -> b
f2f = uncurry encodeFloat . decodeFloat
{-# INLINE f2f #-}

instance Random RSfloat where
    randomR (lo,hi) g = first f2f $ randomR (f2f lo, f2f hi :: Float) g
    random g = first (f2f :: Float -> RSfloat) $ random g

instance Random RSdouble where
    randomR (lo,hi) g = first f2f $ randomR (f2f lo, f2f hi :: Double) g
    random g = first (f2f :: Double -> RSdouble) $ random g

