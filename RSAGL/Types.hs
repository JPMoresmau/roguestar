{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RSAGL.Types
    (RSfloat,RSdouble,toGLfloat,toGLdouble,fromGLfloat,fromGLdouble,f2f,GLUT.GLfloat,GLUT.GLdouble)
    where

import Graphics.UI.GLUT as GLUT
import Data.Vec as Vec
import System.Random
import Control.Arrow

newtype RSfloat = RSfloat GLfloat
    deriving (Enum,Eq,Floating,Fractional,Num,Ord,Read,Real,RealFloat,RealFrac,Show)
newtype RSdouble = RSdouble GLdouble
    deriving (Enum,Eq,Floating,Fractional,Num,Ord,Read,Real,RealFloat,RealFrac,Show)

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

instance NearZero RSfloat where
    nearZero x = abs x < 1e-6
    {-# INLINE nearZero #-}

instance NearZero RSdouble where
    nearZero x = abs x < 1e-14
    {-# INLINE nearZero #-}

instance Random RSfloat where
    randomR (lo,hi) g = first f2f $ randomR (f2f lo, f2f hi :: Float) g
    random g = first (f2f :: Float -> RSfloat) $ random g

instance Random RSdouble where
    randomR (lo,hi) g = first f2f $ randomR (f2f lo, f2f hi :: Double) g
    random g = first (f2f :: Double -> RSdouble) $ random g

