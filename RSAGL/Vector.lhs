\section{Points and Vectors: RSAGL.Vector}

\begin{code}
module RSAGL.Vector
    (Point3D(..),
     origin_point_3d,
     Point2D(..),
     Vector3D(..),
     SurfaceVertex3D(..),
     zero_vector,
     point3d,
     point2d,
     points2d,
     points3d,
     points3d_2,
     vector3d,
     to2d,
     to3d,
     dotProduct,
     angleBetween,
     crossProduct,
     distanceBetween,
     distanceBetweenSquared,
     aNonZeroVector,
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
     randomXYZ)
    where

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

vectorString :: Xyz a => a -> String
vectorString xyz = let (x,y,z) = toXYZ xyz
		       in (show x) ++ "," ++ (show y) ++ "," ++ (show z)

uncurry3d :: (Double -> Double -> Double -> a) -> XYZ -> a
uncurry3d fn (x,y,z) = fn x y z
\end{code}

\subsection{Points in 3-space}

\begin{code}
data Point3D = Point3D !Double !Double !Double
	     deriving (Read,Show,Eq)

origin_point_3d :: Point3D
origin_point_3d = Point3D 0 0 0

point3d :: (Double,Double,Double) -> Point3D
point3d = uncurry3d Point3D

points3d :: [(Double,Double,Double)] -> [Point3D]
points3d = map point3d

points3d_2 :: [((Double,Double,Double),(Double,Double,Double))] -> [(Point3D,Point3D)]
points3d_2 = map (\(l,r) -> (point3d l,point3d r))

instance Xyz Point3D where
    toXYZ (Point3D x y z) = (x,y,z)
    fromXYZ (x,y,z) = Point3D x y z

instance NFData Point3D
\end{code}

\subsection{Points in 2-space}

\begin{code}
data Point2D = Point2D !Double !Double
	     deriving (Read,Show,Eq)

point2d :: (Double,Double) -> Point2D
point2d = uncurry Point2D

points2d :: [(Double,Double)] -> [Point2D]
points2d = map point2d
\end{code}

\subsection{Vectors in 3-space}

\begin{code}
data Vector3D = Vector3D !Double !Double !Double
	      deriving (Read,Show,Eq)

zero_vector :: Vector3D
zero_vector = Vector3D 0 0 0

vector3d :: (Double,Double,Double) -> Vector3D
vector3d = uncurry3d Vector3D

instance Xyz Vector3D where
    toXYZ (Vector3D x y z) = (x,y,z)
    fromXYZ (x,y,z) = Vector3D x y z

instance NFData Vector3D

instance AbstractVector Vector3D where
    zero = zero_vector
    add = vectorAdd
    sub x y = vectorAdd x $ vectorScale (-1) y
    scalarMultiply = vectorScale
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

\subsection{Conversion between 2- and 3- dimensional points}

\begin{code}
to3d :: Point2D -> Point3D
to3d (Point2D x y) = Point3D x y 0

to2d :: Point3D -> Point2D
to2d (Point3D x y 0) = Point2D x y
to2d _ = error "to2d: z is not zero"
\end{code}

\subsection{Vector Arithmetic}

\begin{code}
aNonZeroVector :: Vector3D -> Maybe Vector3D
aNonZeroVector (Vector3D 0 0 0) = Nothing
aNonZeroVector vector = Just vector

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

vectorAdd :: Vector3D -> Vector3D -> Vector3D
vectorAdd (Vector3D ax ay az) (Vector3D bx by bz) = Vector3D (ax+bx) (ay+by) (az+bz)

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

newell calculates the normal vector of an arbitrary polygon.  If the points specified are non-coplanar,
newell often calculates a reasonable result.

The result is a normalized vector.

\begin{code}
newell :: [Point3D] -> Vector3D
newell points = vectorNormalize $ vectorSum $ map newell_ $ loopedDoubles points
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