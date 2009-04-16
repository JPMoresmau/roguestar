\section{Points and Vectors: RSAGL.Vector}

\begin{code}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, PatternGuards #-}
module RSAGL.Vector
    (Point3D(..),
     origin_point_3d,
     Vector3D(..),
     SurfaceVertex3D(..),
     zero_vector,
     point3d,
     points3d,
     point2d,
     points2d,
     vector3d,
     dotProduct,
     angleBetween,
     crossProduct,
     distanceBetween,
     distanceBetweenSquared,
     aNonZeroVector,
     aLargeVector,
     vectorAdd,
     vectorSum,
     vectorScale,
     vectorScaleTo,
     vectorToFrom,
     vectorNormalize,
     vectorAverage,
     vectorLength,
     vectorLengthSquared,
     newell,
     Xyz(..),
     XYZ,
     vectorString,
     randomXYZ,
     fixOrtho,
     fixOrtho2,
     fixOrtho2Left,
     orthos)
    where

import Data.Maybe
import Control.Parallel.Strategies
import RSAGL.Angle
import RSAGL.Auxiliary
import System.Random
import RSAGL.AbstractVector
\end{code}

\subsection{Generic 3-dimensional types and operations}

\begin{code}
type XYZ = (Double,Double,Double)

class Xyz a where
    toXYZ :: a -> XYZ
    fromXYZ :: XYZ -> a

instance Xyz (Double,Double,Double) where
    toXYZ = id
    fromXYZ = id

vectorString :: Xyz a => a -> String
vectorString xyz = let (x,y,z) = toXYZ xyz
		       in (show x) ++ "," ++ (show y) ++ "," ++ (show z)

uncurry3d :: (Double -> Double -> Double -> a) -> XYZ -> a
uncurry3d fn (x,y,z) = fn x y z
\end{code}

\subsection{Points in 3-space}

\begin{code}
data Point3D = Point3D {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
	     deriving (Read,Show,Eq)

origin_point_3d :: Point3D
origin_point_3d = Point3D 0 0 0

point3d :: (Double,Double,Double) -> Point3D
point3d = uncurry3d Point3D

point2d :: (Double,Double) -> Point3D
point2d (x,y) = point3d (x,y,0)

points3d :: [(Double,Double,Double)] -> [Point3D]
points3d = map point3d

points2d :: [(Double,Double)] -> [Point3D]
points2d = map point2d

instance Xyz Point3D where
    toXYZ (Point3D x y z) = (x,y,z)
    fromXYZ (x,y,z) = Point3D x y z

instance AbstractZero Point3D where
    zero = origin_point_3d

instance AbstractAdd Point3D Vector3D where
    add = displace

instance AbstractSubtract Point3D Vector3D where
    sub = vectorToFrom

instance NFData Point3D
\end{code}

\subsection{Vectors in 3-space}

\begin{code}
data Vector3D = Vector3D {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
	      deriving (Read,Show,Eq)

zero_vector :: Vector3D
zero_vector = Vector3D 0 0 0

vector3d :: (Double,Double,Double) -> Vector3D
vector3d = uncurry3d Vector3D

instance Xyz Vector3D where
    toXYZ (Vector3D x y z) = (x,y,z)
    fromXYZ (x,y,z) = Vector3D x y z

instance NFData Vector3D

instance AbstractZero Vector3D where
    zero = zero_vector

instance AbstractAdd Vector3D Vector3D where
    add = vectorAdd

instance AbstractSubtract Vector3D Vector3D where
    sub = vectorToFrom

instance AbstractScale Vector3D where
    scalarMultiply = vectorScale

instance AbstractMagnitude Vector3D where
    magnitude = vectorLength

instance AbstractVector Vector3D
\end{code}

A \texttt{SurfaceVertex3D} is a a point on an orientable surface, having a position and a normal vector.

\subsection{Surface Vertices in 3-space}

\begin{code}
data SurfaceVertex3D = SurfaceVertex3D { sv3d_position :: Point3D, 
                                         sv3d_normal :: Vector3D }
    deriving (Read,Show)

instance NFData SurfaceVertex3D where
    rnf (SurfaceVertex3D p v) = rnf (p,v)
\end{code}

\subsection{Vector Arithmetic}

\begin{code}
aNonZeroVector :: Vector3D -> Maybe Vector3D
aNonZeroVector v = case vectorLength v of
    x | x <= 0 -> Nothing
    x | isDenormalized x -> Nothing
    x | isNaN x -> Nothing
    x | isInfinite x -> Nothing
    _ | otherwise -> Just v

aLargeVector :: Double -> Vector3D -> Maybe Vector3D
aLargeVector x v_ =
    case aNonZeroVector v_ of
        Just v | vectorLength v > x -> Just v
        _ | otherwise -> Nothing

dotProduct :: Vector3D -> Vector3D -> Double
dotProduct (Vector3D ax ay az) (Vector3D bx by bz) = 
    (ax*bx) + (ay*by) + (az*bz)

angleBetween :: Vector3D -> Vector3D -> Angle
angleBetween a b = arcCosine $ dotProduct (vectorNormalize a) (vectorNormalize b)

crossProduct :: Vector3D -> Vector3D -> Vector3D
crossProduct (Vector3D ax ay az) (Vector3D bx by bz) = 
    Vector3D (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

distanceBetween :: (Xyz xyz) => xyz -> xyz -> Double
distanceBetween a b = vectorLength $ vectorToFrom a b

distanceBetweenSquared :: (Xyz xyz) => xyz -> xyz -> Double
distanceBetweenSquared a b = vectorLengthSquared $ vectorToFrom a b

displace :: (Xyz xyz) => xyz -> Vector3D -> xyz
displace xyz (Vector3D x2 y2 z2) =
    let (x1,y1,z1) = toXYZ xyz
        in fromXYZ (x1+x2,y1+y2,z1+z2)

vectorAdd :: Vector3D -> Vector3D -> Vector3D
vectorAdd = displace

vectorSum :: [Vector3D] -> Vector3D
vectorSum vectors = foldr vectorAdd zero_vector vectors

vectorToFrom :: (Xyz xyz) => xyz -> xyz -> Vector3D
vectorToFrom a b = 
    let (ax,ay,az) = toXYZ a
        (bx,by,bz) = toXYZ b
        in Vector3D (ax - bx) (ay - by) (az - bz)

vectorLength :: Vector3D -> Double
vectorLength = sqrt . vectorLengthSquared

vectorLengthSquared :: Vector3D -> Double
vectorLengthSquared (Vector3D x y z) = (x*x + y*y + z*z)

vectorScale :: Double -> Vector3D -> Vector3D
vectorScale s (Vector3D x y z) = Vector3D (x*s) (y*s) (z*s)
\end{code}

vectorScaleTo forces the length of a vector to a certain value, without changing the vector's direction.

\begin{code}
vectorScaleTo :: Double -> Vector3D -> Vector3D
vectorScaleTo new_length vector = vectorScale new_length $ vectorNormalize vector
\end{code}

vectorNormalize forces the length of a vector to 1, without changing the vector's direction.

\begin{code}
vectorNormalize :: Vector3D -> Vector3D
vectorNormalize v = 
    let l = vectorLength v
	in maybe (Vector3D 0 0 0) (vectorScale (1/l)) $ aNonZeroVector v
\end{code}

vectorAverage takes the average of a list of vectors.  The result is a normalized vector
where the length of the element vectors is not reflected in the result.

\begin{code}
vectorAverage :: [Vector3D] -> Vector3D
vectorAverage vects = vectorNormalize $ vectorSum $ map vectorNormalize vects
\end{code}

\subsection{Generating normal vectors}

\texttt{newell} calculates the normal vector of an arbitrary polygon.  If the points specified are non-coplanar,
\texttt{newell} often calculates a reasonable result; if they are colinear or singular \texttt{newell} will fail.

The result is a normalized vector.

\begin{code}
newell :: [Point3D] -> Maybe Vector3D
newell points = fmap vectorNormalize $ aNonZeroVector $ vectorSum $ map newell_ $ loopedDoubles points
    where newell_ (Point3D x0 y0 z0,Point3D x1 y1 z1) =
              (Vector3D 
               ((y0 - y1)*(z0 + z1))
               ((z0 - z1)*(x0 + x1))
               ((x0 - x1)*(y0 + y1)))
\end{code}

\subsection{Randomly Generated Coordinates}

\texttt{randomXYZ} can generate random coordinates within the cube where x, y, and z are each in the range (lo,hi).

\begin{code}
randomXYZ :: (RandomGen g,Xyz p) => (Double,Double) -> g -> (p,g)
randomXYZ lohi g = (fromXYZ (x,y,z),g')
    where (g_,g') = split g
          (x:y:z:_) = randomRs lohi g_
\end{code}

\subsection{Orthagonal Vectors}

\texttt{fixOrtho a v} finds the vector, orthagonal to a, that has the least angle to v.

\texttt{fixOrtho2 right up} yields \texttt{(up,forward)}.

\texttt{fixOrtho2Left right up} yields \texttt{(up,backward)}.

\texttt{orthos} finds two arbitrary vectors orthagonal to the parameter.

\begin{code}
fixOrtho :: Vector3D -> Vector3D -> Vector3D
fixOrtho a = fst . fixOrtho2 a

fixOrtho2 :: Vector3D -> Vector3D -> (Vector3D,Vector3D)
fixOrtho2 a v = (vectorNormalize $ crossProduct a $ vectorScale (-1) b,vectorNormalize b)
    where b = crossProduct a v

fixOrtho2Left :: Vector3D -> Vector3D -> (Vector3D,Vector3D)
fixOrtho2Left a v = (vectorNormalize $ crossProduct a b,vectorNormalize b)
    where b = vectorScale (-1) $ crossProduct a v

orthos :: Vector3D -> (Vector3D,Vector3D)
orthos v@(Vector3D x y z) | abs y >= abs x && abs z >= abs x = fixOrtho2 v (Vector3D (abs x + abs y + abs z) y z)
orthos v@(Vector3D x y z) | abs x >= abs y && abs z >= abs y = fixOrtho2 v (Vector3D x (abs x + abs y + abs z) z)
orthos v@(Vector3D x y z) | abs x >= abs z && abs y >= abs z = fixOrtho2 v (Vector3D x y (abs x + abs y + abs z))
orthos _ = error "orthos: NaN"
\end{code}
