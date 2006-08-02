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
     vector3d,
     crossProduct,
     vectorAdd,
     vectorSum,
     vectorToFrom,
     vectorNormalize,
     vectorAverage,
     normal,
     newell,
     Xyz(..),
     XYZ,
     vectorString,
     transform,
     translationMatrix,
     rotationMatrix,
     translate,
     rotate)
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

point2d :: (Float,Float) -> Point2D
point2d = uncurry Point2D

vector3d :: (Float,Float,Float) -> Vector3D
vector3d = uncurry3d Vector3D

-- |
-- Checks that the given vector is valid (no NaNs, etc)
-- before returning the vector unchanged.
--
vector3Dassert :: String -> Vector3D -> Vector3D
vector3Dassert _ vector = vector
--vector3Dassert msg vector@(Vector3D x y z) =
--    if any isNaN [x,y,z]
--    then error ("vector3Dassert: isNan; " ++ msg)
--    else vector

-- |
-- Checks that the given vector is not zero.
--
vector3Dassert_notZero :: String -> Vector3D -> Vector3D
--vector3Dassert_notZero msg (Vector3D 0 0 0) = error ("vector3Dassert_notZero; " ++ msg)
vector3Dassert_notZero _ vector = vector

-- |
-- The cross product of two vectors.
--
crossProduct :: Vector3D -> Vector3D -> Vector3D
crossProduct (Vector3D ax ay az) (Vector3D bx by bz) = 
    vector3Dassert "crossProduct" $
    Vector3D (ay*bz - az*by) (az*bx - az*bz) (ax*by - ay*bx)

-- |
-- Add two vectors.
--
vectorAdd :: Vector3D -> Vector3D -> Vector3D
vectorAdd (Vector3D ax ay az) (Vector3D bx by bz) = 
    vector3Dassert "vectorAdd" $
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
-- Answers the same vector normalized to a length of 1.
--
vectorNormalize :: Vector3D -> Vector3D
--vectorNormalize (Vector3D 0.0 0.0 0.0) = error "vectorNormalize: no zero vectors, please"
vectorNormalize v@(Vector3D x y z) = 
    let l = vectorLength v
	in vector3Dassert "vectorNormalize" $
	   Vector3D (x/l) (y/l) (z/l)

-- |
-- Takes the average direction of a list of vectors.  The result is a normalized vector,
-- and the length of the element vectors does not factor into the result.
--
vectorAverage :: [Vector3D] -> Vector3D
vectorAverage vects = vectorNormalize $
		      vector3Dassert_notZero "vectorAverage" $
		      vectorSum $ map vectorNormalize vects

-- |
-- The normal vector taken from three points.
--
normal :: Point3D -> Point3D -> Point3D -> Vector3D
normal a b c = vectorSum [((a `vectorToFrom` b) `crossProduct` (c `vectorToFrom` b)),
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
-- cols_outer - the matrix data, where the outer list represents columns (natural form)
-- rows_outer - the matrix data, where the outer list represents rows (transposed form)
--
data Matrix a = Matrix { rows, cols :: Int, cols_outer, rows_outer :: [[a]] }

instance (Show a) => Show (Matrix a) where
    show m = show $ cols_outer m

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
				    cols_outer=dats,
				    rows_outer=transpose dats }
		      else error "row lengths do not match")

-- |
-- Adds two matrices.
--
matrixAdd :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixAdd m n = let new_cols_outer = (if and [rows m == rows n,cols m == cols n]
				      then map ((map (uncurry (+))).(uncurry zip)) $ zip (cols_outer m) (cols_outer n)
				      else error "matrixAdd: dimension mismatch")
		    in Matrix { rows=rows m,cols=cols m,cols_outer=new_cols_outer,rows_outer=transpose new_cols_outer }

-- |
-- Multiply two matrices.
--
matrixMultiply :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixMultiply m n = let m_data = cols_outer m
			 n_data = rows_outer n
			 new_cols_outer = (if (cols m) == (rows n)
					   then [[sum $ map (uncurry (*)) $ zip m' n' | n' <- n_data] | m' <- m_data] 
					   else error "matrixMultiply: dimension mismatch")
			 in Matrix { rows=rows m,
				     cols=cols n,
				     cols_outer = new_cols_outer,
				     rows_outer = transpose new_cols_outer }

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
genericFromHomogenous m = let x = (cols_outer m) !! 0 !! 0
			      y = (cols_outer m) !! 1 !! 0
			      z = (cols_outer m) !! 2 !! 0
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

translate :: (Homogenous a,Homogenous b) => Vector3D -> a -> b
translate vector = transform $ translationMatrix vector

rotate :: (Homogenous a,Homogenous b) => Vector3D -> Float -> a -> b
rotate vector radians = transform $ rotationMatrix vector radians
