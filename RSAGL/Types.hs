module RSAGL.Types
    (RSfloat,RSdouble,f2f,GLUT.GLfloat,GLUT.GLdouble)
    where

import Graphics.UI.GLUT as GLUT
import Data.Vec as Vec
import System.Random
import Control.Arrow

type RSfloat = GLfloat
type RSdouble = GLdouble

{-# RULES
"f2f/id"    f2f = id #-}
f2f :: (RealFloat a,RealFloat b) => a -> b
f2f = uncurry encodeFloat . decodeFloat
{-# INLINE f2f #-}

instance NearZero GLfloat where
    nearZero x = abs x < 1e-6
    {-# INLINE nearZero #-}

instance NearZero GLdouble where
    nearZero x = abs x < 1e-14
    {-# INLINE nearZero #-}

instance Random GLfloat where
    randomR (lo,hi) g = first f2f $ randomR (f2f lo, f2f hi :: Float) g
    random g = first (f2f :: Float -> GLfloat) $ random g

instance Random GLdouble where
    randomR (lo,hi) g = first f2f $ randomR (f2f lo, f2f hi :: Double) g
    random g = first (f2f :: Double -> GLdouble) $ random g

