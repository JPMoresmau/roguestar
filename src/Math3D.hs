--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module Math3D
    (Point3D(..),
     Point2D(..),
     Vector3D(..),
     point3d,
     point2d,
     points2d,
     points3d,
     points3d_2,
     vector3d,
     interpolateBetween3d,
     loopedInterpolateBetween3d,
     crossProduct,
     vectorAdd,
     vectorSum,
     vectorToFrom,
     vectorNormalize,
     vectorAverage,
     planeNormal,
     newell,
     Xyz(..),
     XYZ,
     vectorString,
     matrixAdd,
     matrixMultiply,
     transform,
     aMByNMatrix,
     translationMatrix,
     rotationMatrix,
     scaleMatrix,
     translate,
     rotate,
     scale,
     translates,
     rotates,
     scales,
     Matrix,
     rowMajorForm,
     colMajorForm)
    where

import Data.List
import ListUtils

type XYZ = (Float,Float,Float)

class Xyz a where
    toXYZ :: a -> XYZ

data Point3D = Point3D Float Float Float
	     deriving (Read,Show)

instance Xyz Point3D where
    toXYZ (Point3D x y z) = (x,y,z)

data Point2D = Point2D Float Float
	     deriving (Read,Show)

data Vector3D = Vector3D Float Float Float
	      deriving (Read,Show)

instance Xyz Vector3D where
    toXYZ (Vector3D x y z) = (x,y,z)

-- |
-- Converts a triple to the form "x,y,z".
--
vectorString :: Xyz a => a -> String
vectorString xyz = let (x,y,z) = toXYZ xyz
		       in (show x) ++ "," ++ (show y) ++ "," ++ (show z)

uncurry3d :: (Float -> Float -> Float -> a) -> (Float,Float,Float) -> a
uncurry3d fn (x,y,z) = fn x y z

point3d :: (Float,Float,Float) -> Point3D
point3d = uncurry3d Point3D

interpolateBetween3d :: [Point3D] -> [Point3D]
interpolateBetween3d pts | length pts < 4 = head pts :
					    (concatMap 
					     (\(p1,p2) -> [simpleInterpolateFn p1 p2,p2]) $ 
					    doubles pts)
interpolateBetween3d pts = let len = length pts
			       begin = take 3 pts
			       end = drop (len-2) pts
			       in (take 3 $ interpolateBetween3d begin) ++ 
				      (concatMap (\x -> [goodInterpolateFn x,x !! 2]) $ consecutives 4 pts) ++ 
				      (drop 2 $ interpolateBetween3d end)

loopedInterpolateBetween3d :: [Point3D] -> [Point3D]
loopedInterpolateBetween3d pts | length pts < 4 = (concatMap
						   (\(p1,p2) -> [simpleInterpolateFn p1 p2,p2]) $
						  loopedDoubles pts)
loopedInterpolateBetween3d pts = (concatMap (\x -> [goodInterpolateFn x,x!! 2]) $ loopedConsecutives 4 pts)

simpleInterpolateFn :: Point3D -> Point3D -> Point3D
simpleInterpolateFn (Point3D x0 y0 z0) (Point3D x1 y1 z1) = Point3D ((x0+x1)/2) ((y0+y1)/2) ((z0+z1)/2)

goodInterpolateFn :: [Point3D] -> Point3D
goodInterpolateFn [p0,p1,p2,p3] = 
    let (Point3D x y z) = (simpleInterpolateFn p1 p2)
	simple = vectorAdd (vectorToFrom p1 p0) (vectorToFrom p2 p3)
	scaled = maybe (Vector3D 0 0 0) (vectorScaleTo (vectorLength (vectorToFrom p1 p2) / 9)) $ aNonZeroVector simple
	(Vector3D x' y' z') = if (vectorLength simple < vectorLength scaled)
			      then simple
			      else scaled
	in Point3D (x+x'/2) (y+y'/2) (z+z'/2)
goodInterpolateFn _ = error "goodInterpolateFn: works only on lists of 4"

point2d :: (Float,Float) -> Point2D
point2d = uncurry Point2D

vector3d :: (Float,Float,Float) -> Vector3D
vector3d = uncurry3d Vector3D

points2d :: [(Float,Float)] -> [Point2D]
points2d = map point2d

points3d :: [(Float,Float,Float)] -> [Point3D]
points3d = map point3d

points3d_2 :: [((Float,Float,Float),(Float,Float,Float))] -> [(Point3D,Point3D)]
points3d_2 = map (\(l,r) -> (point3d l,point3d r))

-- |
-- Checks that the given vector is valid (no NaNs, etc)
-- before returning the vector unchanged.
--
aValidVector :: String -> Vector3D -> Vector3D
aValidVector msg vector@(Vector3D x y z) =
    if any isNaN [x,y,z]
    then error ("aValidVector failed: isNan; " ++ msg)
    else vector

-- |
-- Checks that the given vector is not zero.
--
aNonZeroVector :: Vector3D -> Maybe Vector3D
aNonZeroVector (Vector3D 0 0 0) = Nothing
aNonZeroVector vector = Just vector

-- |
-- The cross product of two vectors.
--
crossProduct :: Vector3D -> Vector3D -> Vector3D
crossProduct (Vector3D ax ay az) (Vector3D bx by bz) = 
    Vector3D (ay*bz - az*by) (az*bx - az*bz) (ax*by - ay*bx)

-- |
-- Add two vectors.
--
vectorAdd :: Vector3D -> Vector3D -> Vector3D
vectorAdd (Vector3D ax ay az) (Vector3D bx by bz) = 
    aValidVector "in vectorAdd" $
    Vector3D (ax+bx) (ay+by) (az+bz)

-- |
-- The sum of a list of vectors.
--
vectorSum :: [Vector3D] -> Vector3D
vectorSum vectors = foldr vectorAdd (Vector3D 0 0 0) vectors

-- |
-- The vector to the first point from the second.
-- 
vectorToFrom :: Point3D -> Point3D -> Vector3D
vectorToFrom (Point3D ax ay az) (Point3D bx by bz) = Vector3D (ax - bx) (ay - by) (az - bz)

-- |
-- Length of the vector.
--
vectorLength :: Vector3D -> Float
vectorLength (Vector3D x y z) = sqrt (x*x + y*y + z*z)

-- |
-- Multiply a vector by a scalar value.
--
vectorScale :: Float -> Vector3D -> Vector3D
vectorScale s (Vector3D x y z) = Vector3D (x*s) (y*s) (z*s)

-- |
-- Scale a vector so that it has the specified length.
--
vectorScaleTo :: Float -> Vector3D -> Vector3D
vectorScaleTo new_length vector = vectorScale new_length $ vectorNormalize vector

-- |
-- Answers the same vector normalized to a length of 1.
--
vectorNormalize :: Vector3D -> Vector3D
vectorNormalize v = 
    let l = vectorLength v
	in maybe (Vector3D 0 0 0) (vectorScale (1/l)) $ aNonZeroVector v

-- |
-- Takes the average direction of a list of vectors.  The result is a normalized vector,
-- and the length of the element vectors does not factor into the result.
--
vectorAverage :: [Vector3D] -> Vector3D
vectorAverage vects = maybe (Vector3D 0 0 0) vectorNormalize $
		      aNonZeroVector $ vectorSum $ map vectorNormalize vects

-- |
-- The normal vector taken from three points.
--
planeNormal :: Point3D -> Point3D -> Point3D -> Vector3D
planeNormal a b c = vectorSum [((a `vectorToFrom` b) `crossProduct` (c `vectorToFrom` b)),
			       ((b `vectorToFrom` c) `crossProduct` (a `vectorToFrom` c)),
			       ((c `vectorToFrom` a) `crossProduct` (b `vectorToFrom` a))]

-- |
-- The normal vector taken from an arbitrary number of points.
--
newell :: [Point3D] -> Vector3D
newell points = vectorSum $ map newell_ $ loopedDoubles points

newell_ :: (Point3D,Point3D) -> Vector3D
newell_ (Point3D x0 y0 z0,Point3D x1 y1 z1) =
    (Vector3D 
     ((y0 - y1)*(z0 + z1))
     ((z0 - z1)*(x0 + x1))
     ((x0 - x1)*(y0 + y1)))

-- |
-- A basic Matrix data structure.
-- rows - the number of rows in the matrix;
-- cols - the number of columns in the matrix;
-- row_major - the matrix data, where the outer list represents columns (natural form)
-- col_major - the matrix data, where the outer list represents rows (transposed form)
--
data Matrix a = Matrix { rows, cols :: Int, row_major, col_major :: [[a]] }

instance (Show a) => Show (Matrix a) where
    show m = show $ row_major m

-- |
-- Constructs a matrix from list form.  The outer list represents columns, the inner list represents
-- rows.  (Such a list form can be formatted correctly in monospaced font and haskell syntax, so that
-- it looks like a matrix as it would be normally written.)
--
matrix :: [[a]] -> Matrix a
matrix dats = let row_lengths = map length dats
		  row_length = head row_lengths
		  in (if all (== row_length) row_lengths
		      then Matrix { rows=length row_lengths, --the number of rows is the length of each column
				    cols=row_length, --the number of columns is the length of each row
				    row_major=dats,
				    col_major=transpose dats }
		      else error "row lengths do not match")

rowMajorForm :: Matrix a -> [[a]]
rowMajorForm mat = row_major mat

colMajorForm :: Matrix a -> [[a]]
colMajorForm mat = col_major mat

aMByNMatrix :: String -> Int -> Int -> Matrix a -> Matrix a
aMByNMatrix msg m n mat = if (m == rows mat && n == cols mat)
			  then mat
			  else error ("aMByNMatrix: not a " ++ show m ++ " by " ++ show n ++ " matrix; " ++ msg)

-- |
-- Adds two matrices.
--
matrixAdd :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixAdd m n = let new_row_major = (if and [rows m == rows n,cols m == cols n]
				      then map ((map (uncurry (+))).(uncurry zip)) $ zip (row_major m) (row_major n)
				      else error "matrixAdd: dimension mismatch")
		    in Matrix { rows=rows m,cols=cols m,row_major=new_row_major,col_major=transpose new_row_major }

-- |
-- Multiply two matrices.
--
matrixMultiply :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixMultiply m n = let m_data = row_major m
			 n_data = col_major n
			 new_row_major = (if (cols m) == (rows n)
					  then [[sum $ map (uncurry (*)) $ zip m' n' | n' <- n_data] | m' <- m_data] 
					  else error "matrixMultiply: dimension mismatch")
			 in Matrix { rows=rows m,
				     cols=cols n,
				     row_major = new_row_major,
				     col_major = transpose new_row_major }

class Homogenous a where
    toHomogenous :: a -> Matrix Float
    fromHomogenous :: Matrix Float -> a

instance Homogenous Vector3D where
    toHomogenous (Vector3D x y z) = matrix [[x],[y],[z],[0.0]]
    fromHomogenous m = uncurry3d Vector3D $ genericFromHomogenous m 

instance Homogenous Point2D where
    toHomogenous (Point2D x y) = matrix [[x],[y],[0.0],[1.0]]
    fromHomogenous m = case (genericFromHomogenous m) of
						      (x,y,_) -> Point2D x y

instance Homogenous Point3D where
    toHomogenous (Point3D x y z) = matrix [[x],[y],[z],[1.0]]
    fromHomogenous m = uncurry3d Point3D $ genericFromHomogenous m

genericFromHomogenous :: Matrix Float -> (Float,Float,Float)
genericFromHomogenous m = let x = (row_major m) !! 0 !! 0
			      y = (row_major m) !! 1 !! 0
			      z = (row_major m) !! 2 !! 0
			      in (x,y,z)

transform :: (Homogenous a, Homogenous b) => Matrix Float -> a -> b
transform transformation_matrix entity = fromHomogenous $ matrixMultiply transformation_matrix $ toHomogenous entity

translationMatrix :: Vector3D -> Matrix Float
translationMatrix (Vector3D x y z) = matrix [[1,0,0,x],
					     [0,1,0,y],
					     [0,0,1,z],
					     [0,0,0,1]]

-- |
-- Answers a Rotation matrix that rotates any object around the specified
-- vector a number of degrees equal to the second paramter, in radians.
--
rotationMatrix :: Vector3D -> Float -> Matrix Float
rotationMatrix vector radians = let s = sin radians
				    c = cos radians
				    c' = 1 - c
				    (Vector3D x y z) = vectorNormalize vector
				    in matrix [[c+c'*x*x,     c'*y*x - s*z,   c'*z*x + s*y, 0],
					       [c'*x*y+s*z,   c+c'*y*y,       c'*z*y - s*x, 0],
					       [c'*x*z-s*y,   c'*y*z+s*x,     c+c'*z*z,     0],
					       [0,            0,              0,            1]]

scaleMatrix :: Vector3D -> Matrix Float
scaleMatrix (Vector3D x y z) = matrix [[x, 0, 0, 0],
				       [0, y, 0, 0],
				       [0, 0, z, 0],
				       [0, 0, 0, 1]]

translate :: (Homogenous a,Homogenous b) => Vector3D -> a -> b
translate vector = transform $ translationMatrix vector

rotate :: (Homogenous a,Homogenous b) => Vector3D -> Float -> a -> b
rotate vector radians = transform $ rotationMatrix vector radians

scale :: (Homogenous a,Homogenous b) => Vector3D -> a -> b
scale vector = transform $ scaleMatrix vector

translates :: (Homogenous a,Homogenous b) => Vector3D -> [a] -> [b]
translates vector = map (translate vector)

rotates :: (Homogenous a,Homogenous b) => Vector3D -> Float -> [a] -> [b]
rotates vector radians = map (rotate vector radians)

scales :: (Homogenous a,Homogenous b) => Vector3D -> [a] -> [b]
scales vector = map (scale vector)