\section{Generating Noise: RSAGL.Noise}
\ref{RSAGLNoise}

\begin{code}
module RSAGL.Noise
    (perlinTurbulence,perlinNoise)
    where

import RSAGL.Interpolation
import RSAGL.Vector
import Data.Array.Unboxed
import Data.Fixed

perlinTurbulence :: Double -> Point3D -> Point3D
perlinTurbulence s (Point3D x y z) = Point3D (x + s*perlinNoise x') (y + s*perlinNoise y') (z + s*perlinNoise z')
    where x' = Point3D (x+100) y z
          y' = Point3D x (y+100) z
          z' = Point3D x y (z+100)

-- |
-- Intellectual property note:
--
-- This is based on Perlin's improved noise reference implementation.
-- I'm assuming from the manner in which it is published and the usage
-- of the term "reference implementation" that it was intended to be
-- the basis of derivative code such as this.
--
perlinNoise :: Point3D -> Double
perlinNoise (Point3D x0 y0 z0) =
   let (x,x') = divMod' x0 1 :: (Int,Double)
       (y,y') = divMod' y0 1 :: (Int,Double)
       (z,z') = divMod' z0 1 :: (Int,Double)
       (u,v,w) = (fade x',fade y',fade z')
       a = pRandom x + y
       aa = pRandom a + z
       ab = pRandom (a+1) + z
       b = pRandom (x+1) + y
       ba = pRandom b + z
       bb = pRandom (b+1) + z
       f n = let nn = n in nn / (1 + abs nn)  -- this function forces the result into the range -1..1
       in f $ lerp w
            (lerp v (lerp u (grad (pRandom aa) x' y' z', grad (pRandom ba) (x'-1) y' z'),
                     lerp u (grad (pRandom ab) x' (y'-1) z', grad (pRandom bb) (x'-1) (y'-1) z')),
             lerp v (lerp u (grad (pRandom $ aa + 1) x' y' (z'-1), grad (pRandom $ ba + 1) (x'-1) y' (z'-1)),
                     lerp u (grad (pRandom $ ab + 1) x' (y'-1) (z'-1), grad (pRandom $ bb + 1) (x'-1) (y'-1) (z'-1))))


pRandom :: Int -> Int
pRandom n = ps ! (n `mod` 256)

ps :: UArray Int Int
ps = listArray (0,255) [226,110,184,248,226,248,61,22,185,81,167,22,54,100,244,77,84,86,184,134,88,117,66,3,53,77,172,108,151,115,131,61,142,31,0,135,12,245,73,119,186,
  203,6,7,39,253,184,133,99,118,230,220,143,113,44,59,249,81,241,125,115,136,58,254,8,193,84,221,83,188,122,76,88,121,246,82,20,142,121,167,27,181,145,73,13,1521,165,100,
  150,197,87,45,171,151,138,40,14,230,81,162,84,10,138,20,43,70,78,18,77,43,108,177,108,102,135,220,136,11,77,234,62,32,156,190,56,44,168,177,105,143,236,255,181,203,67,
  137,31,28,199,214,47,177,61,81,205,25,138,215,6,251,53,125,102,177,30,28,210,50,255,18,71,230,215,158,58,137,3,130,105,183,226,0,159,71,75,132,254,211,158,186,196,33,
  20,72,55,74,241,165,118,67,56,118,70,82,22,159,177,126,242,102,97,213,63,22,69,68,248,247,46,148,202,228,200,242,4,29,251,71,46,222,57,33,179,226,147,203,166,38,29,96,
  224,246,11,32,94,205,108,128,63,138,252,166,62,24,215,109,165,135,53,166,5,139,185,25,68]

fade :: Double -> Double
fade t = t ^ 3 * (t * (t * 6 - 15) + 10)

grad :: Int -> Double -> Double -> Double -> Double
grad hash x y z = 
    case hash `mod` 12 of
                       0 -> x + y
                       1 -> (-x) + y
                       2 -> x + (-y)
                       3 -> (-x) + (-y)
                       4 -> z + y
                       5 -> (-z) + y
                       6 -> z + (-y)
                       7 -> (-z) + (-y)
                       8 -> z + x
                       9 -> (-z) + x
                       10 -> z + (-x)
                       11 -> (-z) + (-x)
                       _ -> error "grad: impossible case"

--
-- End of the Perlin noise functions derived from the reference implementation.
--
\end{code}