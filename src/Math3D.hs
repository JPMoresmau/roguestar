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

{-# OPTIONS_GHC -fglasgow-exts #-}

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
     origin_point_3d,
     Point2D(..),
     Vector3D(..),
     zero_vector,
     point3d,
     point2d,
     points2d,
     points3d,
     points3d_2,
     vector3d,
     interpolateBetween3d,
     interpolateBetween2d,
     loopedInterpolateBetween3d,
     dotProduct,
     angleBetween,
     crossProduct,
     distanceBetween,
     vectorAdd,
     vectorSum,
     vectorScale,
     vectorScaleTo,
     vectorToFrom,
     vectorNormalize,
     vectorAverage,
     vectorLength,
     planeNormal,
     newell,
     Xyz(..),
     XYZ,
     vectorString,
     matrix,
     identityMatrix,
     xyzMatrix,
     matrixAdd,
     matrixMultiply,
     matrixTranspose,
     matrixInverse,
     AffineTransformable(..),
     transformHomogenous,
     aMByNMatrix,
     translationMatrix,
     rotationMatrix,
     scaleMatrix,
     translate,
     rotate,
     scale,
     Matrix,
     rowMajorForm,
     colMajorForm,
     NoiseFunction,
     noiseAt,
     composeNoiseFunctions,
     noiseFunction,
     perlin_noise_function,
     synthesizePerlinNoise,
     Lerpable(..),
     lerpBetween,
     lerpMap)
    where

import Data.List
import Data.Array
import Data.Maybe
import Control.Monad
import ListUtils
import System.Random
                
type XYZ = (Float,Float,Float)

class Xyz a where
    toXYZ :: a -> XYZ

data Point3D = Point3D Float Float Float
	     deriving (Read,Show,Eq)

origin_point_3d :: Point3D
origin_point_3d = Point3D 0 0 0

instance Xyz Point3D where
    toXYZ (Point3D x y z) = (x,y,z)

data Point2D = Point2D Float Float
	     deriving (Read,Show,Eq)

data Vector3D = Vector3D Float Float Float
	      deriving (Read,Show,Eq)

zero_vector :: Vector3D
zero_vector = Vector3D 0 0 0
                
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
					     (\(p1,p2) -> [lerp (0.5 :: Float) (p1,p2),p2]) $ 
					    doubles pts)
interpolateBetween3d pts = let len = length pts
			       begin = take 3 pts
			       end = drop (len-2) pts
			       in (take 3 $ interpolateBetween3d begin) ++ 
				      (concatMap (\x -> [goodInterpolateFn x,x !! 2]) $ consecutives 4 pts) ++ 
				      (drop 2 $ interpolateBetween3d end)

loopedInterpolateBetween3d :: [Point3D] -> [Point3D]
loopedInterpolateBetween3d pts | length pts < 4 = (concatMap
						   (\(p1,p2) -> [lerp (0.5 :: Float) (p1,p2),p2]) $
						  loopedDoubles pts)
loopedInterpolateBetween3d pts = (concatMap (\x -> [goodInterpolateFn x,x !! 2]) $ loopedConsecutives 4 pts)

interpolateBetween2d :: [Point2D] -> [Point2D]
interpolateBetween2d = map to2d . interpolateBetween3d . map to3d

goodInterpolateFn :: [Point3D] -> Point3D
goodInterpolateFn [p0,p1,p2,p3] = 
    let (Point3D x y z) = lerp (0.5 :: Float) (p1,p2)
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

to3d :: Point2D -> Point3D
to3d (Point2D x y) = Point3D x y 0

to2d :: Point3D -> Point2D
to2d (Point3D x y 0) = Point2D x y
to2d _ = error "to2d: z is not zero"

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
-- The dot product of two vectors.
--
dotProduct :: Vector3D -> Vector3D -> Float
dotProduct (Vector3D ax ay az) (Vector3D bx by bz) = 
    (ax*bx) + (ay*by) + (az*bz)

-- |
-- The angle between two vectors, based on their dot product, in radians.
--
angleBetween :: Vector3D -> Vector3D -> Float
angleBetween a b = acos $ dotProduct (vectorNormalize a) (vectorNormalize b)

-- |
-- The cross product of two vectors.
--
crossProduct :: Vector3D -> Vector3D -> Vector3D
crossProduct (Vector3D ax ay az) (Vector3D bx by bz) = 
    Vector3D (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

-- |
-- The distance between two points.
--
distanceBetween :: Point3D -> Point3D -> Float
distanceBetween a b = vectorLength $ vectorToFrom a b
                
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
data Matrix a = Matrix { rows, cols :: Integer, row_major, col_major :: [[a]] }

instance (Show a) => Show (Matrix a) where
    show m = show $ row_major m
                
-- |
-- Constructs a matrix from list form.  The outer list represents columns, the inner lists represent
-- rows.  (Such a list form can be formatted correctly in monospaced font and haskell syntax, so that
-- it looks like a matrix as it would be normally written.)
--
matrix :: [[a]] -> Matrix a
matrix dats = let row_lengths = map genericLength dats
		  row_length = head row_lengths
		  in (if all (== row_length) row_lengths
		      then Matrix { rows=genericLength row_lengths, --the number of rows is the length of each column
				    cols=row_length, --the number of columns is the length of each row
				    row_major=dats,
				    col_major=transpose dats }
		      else error "row lengths do not match")

-- |
-- identityMatrix n is the n by n identity matrix
--
identityMatrix :: (Num a,Integral i) => i -> Matrix a
identityMatrix n = matrix $ map (\x -> genericReplicate x 0 ++ [1] ++ genericReplicate (n-1-x) 0) [0..n-1]

-- |
-- Produces the matrix in which the x y and z axis are transformed to point in the direction of the specified
-- vectors.
--
xyzMatrix :: Vector3D -> Vector3D -> Vector3D -> Matrix Float
xyzMatrix (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) (Vector3D x3 y3 z3) =
    matrix [[x1,x2,x3,0],
            [y1,y2,y3,0],
            [z1,z2,z3,0],
            [0,0,0,0]]
                
rowMajorForm :: Matrix a -> [[a]]
rowMajorForm mat = row_major mat

colMajorForm :: Matrix a -> [[a]]
colMajorForm mat = col_major mat

aMByNMatrix :: String -> Integer -> Integer -> Matrix a -> Matrix a
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

-- |
-- Transpose a matrix.
--
matrixTranspose :: Matrix a -> Matrix a
matrixTranspose = matrix . colMajorForm -- works because matrix expects row major form
                
-- |
-- Returns the same matrix, with rows that have more leading zeroes below rows that have fewer.
-- Used for Gaussian elimination.
--
sortMatrixByLeadingZeroes :: (Num a) => Matrix a -> Matrix a
sortMatrixByLeadingZeroes m =
    let leadingZeroCount r = length $ takeWhile (== 0) r
        compareByLeadingZeroes l r = compare (leadingZeroCount l) (leadingZeroCount r)
        in matrix $ sortBy compareByLeadingZeroes (rowMajorForm m)

-- |
-- Picks a row of a matrix matching the specified, which requires
-- the specified number of leading zeroes, a non-zero value, and
-- then a specified number of trailing zeroes.  There may be additional
-- arbitrary trailing values after the trailing zeroes.
--
-- Used for Gaussian elimination.
--
pickRowMatching :: (Num a,Fractional a) => Integer -> Integer -> Matrix a -> [a] -> Maybe [a]
pickRowMatching leading trailing m nonmatching =
    let rowMatches r = (all (== 0) $ genericTake leading r) && 
                       (all (== 0) $ genericTake trailing $ genericDrop (leading + 1) r) &&
                       ((r `genericIndex` leading) /= 0) &&
                       (not $ areScalarMultiples nonmatching r)
        areScalarMultiples l r = let ratio_list = zipWith (\l' r' -> if r' == 0 then Nothing else Just $ l' / r') l r
                                     one_ratio = fromMaybe Nothing $ find isJust ratio_list
                                     in (isJust one_ratio) && (all (== one_ratio) ratio_list)
        in find rowMatches $ rowMajorForm m

-- |
-- Forces the specified matrix row to have the specified number of
-- leading zeroes, using Gauss-Jordan row matrix operations.
--
-- An operation for Gaussian elimination.
--
forceLeadingZeroesAtRow :: Integer -> Integer -> Matrix Rational -> Maybe (Matrix Rational)
forceLeadingZeroesAtRow row zeroes m =
    let forceLeadingZeroAt row' m' column' = 
          let original_row = m' `rowAt` row'
              in if (original_row `genericIndex` column' /= 0)
                 then do usable_row <- pickRowMatching column' 0 m' original_row
                         let ratio = (original_row `genericIndex` column') / (usable_row `genericIndex` column')
                             new_row = zipWith (-) original_row $ map (* ratio) usable_row
                         return $ replaceRow m' row' new_row
                 else Just m
    in foldM (forceLeadingZeroAt row) m [0..(zeroes-1)]
                
-- |
-- Use Gaussian elimination to produce an upper triangular matrix.
--
toUpperTriangularMatrix :: (Num a,Real a,Fractional a) => Matrix a -> Maybe (Matrix a)
toUpperTriangularMatrix m = 
    liftM (coerceMatrix fromRational) $ foldM (\m' n -> forceLeadingZeroesAtRow n n m') (sortMatrixByLeadingZeroes $ coerceMatrix toRational m) [0..(min (rows m) (cols m) - 1)]

reduceMatrix :: Matrix a -> (Integer,Integer) -> Matrix a
reduceMatrix m (i,j) =
    let (above,below) = genericSplitAt j $ rowMajorForm m
        (left,right) = genericSplitAt i $ transpose $ above ++ tail below
        in matrix $ transpose $ left ++ tail right
                
matrixMinor :: (Num a,Real a,Fractional a) => Matrix a -> (Integer,Integer) -> a
matrixMinor m ij = determinant $ reduceMatrix m ij

-- |
-- The cofactor of m at (i,j) is the element at (i,j) times the determinant of
-- matrix m with row i and column j removed.
--
matrixCofactor :: (Num a,Fractional a,Real a) => Matrix a -> (Integer,Integer) -> a
matrixCofactor m (0,0) | rows m == 1 && cols m == 1 = matrixAt m (0,0)
matrixCofactor m (i,j) = (-1)^(i+j) * matrixMinor m (i,j)

-- |
-- Answers the inverse of this matrix.
--
matrixInverse :: (Num a,Fractional a,Real a) => Matrix a -> Matrix a
matrixInverse m | rows m /= cols m = error "matrixInverse: not a square matrix"
matrixInverse m | determinant m == 0 = error "matrixInverse: det m = 0"
matrixInverse m = 
    let scale_factor = 1 / determinant m
        in matrixTranspose $ matrix [[scale_factor * matrixCofactor m (i,j) | i <- [0..(cols m-1)]]
                                        | j <- [0..(rows m-1)]]

-- |
-- Coerce a matrix from one type to another.
--
coerceMatrix :: (a -> b) -> Matrix a -> Matrix b
coerceMatrix fn m = matrix $ map (map fn) $ rowMajorForm m

-- |
-- Answers the nth row of a matrix.
--
rowAt :: Matrix a -> Integer -> [a]
rowAt m n = (rowMajorForm m) `genericIndex` n

matrixAt :: Matrix a -> (Integer,Integer) -> a
matrixAt m (i,j) = ((rowMajorForm m) `genericIndex` j) `genericIndex` i

-- |
-- Replace a specific row in a matrix.
--
replaceRow :: Matrix a -> Integer -> [a] -> Matrix a
replaceRow m n new_row =
    let (begin,rest) = genericSplitAt n $ rowMajorForm m
        in matrix $ begin ++ [new_row] ++ (tail rest)

determinant :: (Num a,Fractional a,Real a) => Matrix a -> a
determinant m | rows m /= cols m = error "determinant: not a square matrix"
determinant m =
    fromMaybe 0 $ do m_upper_triangular <- toUpperTriangularMatrix m
                     return $ product $ zipWith genericIndex (rowMajorForm m_upper_triangular) [0..]

-- |
-- Class for entities such as points and vectors that can be represented as matrices.
-- toHomogenous always results in a column matrix, while fromHomogenous always
-- expects a row matrix.
--
-- This means that (fromHomogenous . toHomogenous) is not an identity function.
-- Instead, (fromHomogenous . matrixTranspose . toHomogenous) would be the identity function.
--
class Homogenous a where
    toHomogenous :: a -> Matrix Float
    fromHomogenous :: Matrix Float -> a

instance Homogenous Vector3D where
    toHomogenous (Vector3D x y z) = matrix [[x],
                                            [y],
                                            [z],
                                            [0.0]]
    fromHomogenous m = uncurry3d Vector3D $ genericFromHomogenous m 

instance Homogenous Point2D where
    toHomogenous (Point2D x y) = matrix [[x],
                                         [y],
                                         [0.0],
                                         [1.0]]
    fromHomogenous m = case (genericFromHomogenous m) of
						      (x,y,_) -> Point2D x y

instance Homogenous Point3D where
    toHomogenous (Point3D x y z) = matrix [[x],
                                           [y],
                                           [z],
                                           [1.0]]
    fromHomogenous m = uncurry3d Point3D $ genericFromHomogenous m

genericFromHomogenous :: Matrix Float -> (Float,Float,Float)
genericFromHomogenous m = let x = (row_major m) !! 0 !! 0
			      y = (row_major m) !! 1 !! 0
			      z = (row_major m) !! 2 !! 0
			      in (x,y,z)
                
class AffineTransformable a where
    transform :: Matrix Float -> a -> a
                
instance AffineTransformable Vector3D where
    transform = transformHomogenous

instance AffineTransformable Point3D where
    transform = transformHomogenous
                
instance AffineTransformable Point2D where
    transform = transformHomogenous
                
transformHomogenous :: (Homogenous a, Homogenous b) => Matrix Float -> a -> b
transformHomogenous transformation_matrix entity = fromHomogenous $ matrixMultiply transformation_matrix $ toHomogenous entity

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

translate :: (AffineTransformable a) => Vector3D -> a -> a
translate vector = transform $ translationMatrix vector

rotate :: (AffineTransformable a) => Vector3D -> Float -> a -> a
rotate vector radians = transform $ rotationMatrix vector radians

scale :: (AffineTransformable a) => Vector3D -> a -> a
scale vector = transform $ scaleMatrix vector
                
data NoiseFunction = NoiseFunction { nf_transformation_in :: Matrix Float,
                                     nf_transformation_out :: Float,
                                     nf_function :: (Matrix Float,Point3D) -> Float }

instance AffineTransformable NoiseFunction where
    transform m nf = nf { nf_transformation_in = (matrixInverse m) `matrixMultiply` (nf_transformation_in nf) }

noiseAt :: NoiseFunction -> (Matrix Float,Point3D) -> Float
noiseAt nf (m,p) = nf_transformation_out nf * (nf_function nf $ (nf_transformation_in nf `matrixMultiply` m,p))

-- |
-- Compose noise functions by (function,amplitude) by addition.  If the amplitudes
-- do not sum to 1, they will scaled thereto so that each amplitude represents the fraction
-- of the result that is contributed by that layer.
--
composeNoiseFunctions :: [(NoiseFunction,Float)] -> NoiseFunction
composeNoiseFunctions nfs =
    let total_amplitude_adjustment = 1 / (sum $ map (abs . snd) nfs)
        new_nfs = map (\(nf,amp) -> nf { nf_transformation_out = nf_transformation_out nf * amp }) nfs
        in NoiseFunction { nf_transformation_in = identityMatrix 4,
                           nf_transformation_out = total_amplitude_adjustment,
                           nf_function = \x -> sum $ map (flip noiseAt x) new_nfs }

noiseFunction :: (Point3D -> Float) -> NoiseFunction
noiseFunction fn = NoiseFunction { nf_transformation_in = identityMatrix 4,
                                   nf_transformation_out = 1.0,
                                   nf_function = \(m,pt) -> fn $ transform m pt }

perlin_noise_function :: NoiseFunction
perlin_noise_function = noiseFunction perlinNoise

-- |
-- Given a range of lengths, synthesize a noise function from
-- layers of noise, with each layer having a frequency a given multiple
-- of the layer before it, so that the synthesized noise function contains
-- layers with many frequencies that are visible at those lengths.
--
synthesizePerlinNoise :: Float -> (Float,Float) -> NoiseFunction
synthesizePerlinNoise _ (l,r) | l > r = error "synthesizePerlinNoise: right length greater than left"
synthesizePerlinNoise m (l,r) = 
    let middle = sqrt $ l * r
        left_lengths = takeWhile (\x -> (x > l) && (x < r)) $ tail $ iterate (/ m) middle
        right_lengths = takeWhile (\x -> (x > l) && (x < r)) $ tail $ iterate (* m) middle
        lengths = left_lengths ++ [middle] ++ right_lengths
        perlin_layers = map (\x -> (scale (Vector3D x x x) perlin_noise_function,x)) lengths
        in composeNoiseFunctions perlin_layers

-- |
-- Class of things subject to linear interpolation.
--
-- lerp u a b, with u at or between zero and unity according to its type,
-- is an entity some fraction of the distance between a b, with u=0
-- indicating a and u=1 indicating b.  If reasonable for the types
-- involved, u may be outside of the zero and unity, with a negative
-- u indicating a point on the opposite side of a from b and with
-- a u > 1 indicating a point on the opposite side of b from a.
--
class Fractional n => Lerpable a n where
    lerp :: n -> (a,a) -> a

-- |
-- lerp takes a parameter between 0 and 1, while lerpBetween takes a parameter between two arbitrary values.
-- lerp u (a,b) == lerpBetween (0,u,1) (a,b)
--
lerpBetween :: (Lerpable a n) => (n,n,n) -> (a,a) -> a
lerpBetween (l,u,r) = lerp $ (u-l) / (r-l)

-- |
-- Given many entities, lerp between the two entities closest to the given point
-- on either side.  For example, if we wanted to lerp between colors on a rainbow,
-- we might use the map [(0,RED),(1,ORANGE),(2,YELLOW),(3,GREEN),(4,BLUE),(5,INDIGO),(6,VIOLET)].
-- lerpMap 3.5 would result in a blue-green color.
--
lerpMap :: (Ord n,Lerpable a n) => n -> [(n,a)] -> a
lerpMap u pts = 
    let (l,l') = minimumBy (\x -> \y -> compare (fst x) (fst y)) $ filter ((>= u) . fst) pts
        (r,r') = maximumBy (\x -> \y -> compare (fst x) (fst y)) $ filter ((<= u) . fst) pts
        in lerpBetween (l,u,r) (l',r')
                
lerpNumber :: (Num n) => n -> (n,n) -> n
lerpNumber u (a,b) = (1-u)*a + u*b

instance Lerpable Float Float where
   lerp = lerpNumber

instance Lerpable Vector3D Float where
   lerp u (Vector3D ax ay az,Vector3D bx by bz) = Vector3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point3D Float where
   lerp u (Point3D ax ay az,Point3D bx by bz) = Point3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point2D Float where
   lerp u (Point2D ax ay,Point2D bx by) = Point2D (lerp u (ax,bx)) (lerp u (ay,by))
                
instance (Lerpable a n) => Lerpable (Maybe a) n where
   lerp u (a,b) = liftM2 (curry $ lerp u) a b
                
-- |
-- Intellectual property note:
--
-- This is based on Perlin's improved noise reference implementation.
-- I'm assuming from the manner in which it is published and the usage
-- of the term "reference implementation" that it was intended to be
-- the basis of derivative code such as this.  If this assumption of
-- Perlin's intent is false, regardless of questions of law, then the
-- following section of code should be replaced with some other
-- noise function.
--
perlinNoise :: Point3D -> Float
perlinNoise (Point3D x0 y0 z0) =
   let toFloorForm (int_part,frac_part) = if frac_part < 0
                                          then (int_part - 1,1+frac_part)
                                          else (int_part,frac_part)
       (x,x') = toFloorForm $ properFraction x0
       (y,y') = toFloorForm $ properFraction y0
       (z,z') = toFloorForm $ properFraction z0
       (u,v,w) = (fade x',fade y',fade z')
       a = pRandom x + y
       aa = pRandom a + z
       ab = pRandom (a+1) + z
       b = pRandom (x+1) + y
       ba = pRandom b + z
       bb = pRandom (b+1) + z
       in lerp w
          (lerp v (lerp u (grad (pRandom aa) x' y' z', grad (pRandom ba) (x'-1) y' z'),
                   lerp u (grad (pRandom ab) x' (y'-1) z', grad (pRandom bb) (x'-1) (y'-1) z')),
           lerp v (lerp u (grad (pRandom $ aa + 1) x' y' (z'-1), grad (pRandom $ ba + 1) (x'-1) y' (z'-1)),
                   lerp u (grad (pRandom $ ab + 1) x' (y'-1) (z'-1), grad (pRandom $ bb + 1) (x'-1) (y'-1) (z'-1))))
          / 0.75 / 2 + 0.5
                  
pRandom :: Integer -> Integer
pRandom n = ps ! (n `mod` 512)

ps :: Array Integer Integer
ps = listArray (0,511) [646370,406894,78264,638200,196322,307448,596541,107030,4793,565585,576423,650262,571446,43108,
                        633076,111437,2644,155222,536760,617862,571480,161909,102722,312579,206645,141133,181676,284524,
                        280215,636019,266883,234557,625806,272415,484352,207495,663564,676597,612937,326007,40890,184011,
                        446726,644027,65575,547325,524216,370821,679779,512886,329446,263644,343183,308593,182572,422971,
                        563193,307537,397553,465789,106099,281992,578618,644862,655624,650433,393044,343517,621395,660924,
                        599418,211276,348504,413817,342262,6994,365076,574606,212345,556199,307227,6325,122769,23113,
                        592141,253080,125009,220837,169572,293782,441541,492375,5933,457899,92823,109194,14120,51982,
                        542438,458577,593826,397140,422154,234122,143636,408107,222278,698702,2578,59981,682795,453740,
                        173489,575340,153190,196999,691932,605064,452107,116045,513002,367422,522528,498588,231102,672824,
                        617004,445864,87729,347241,541839,389100,536575,274357,631243,688451,622729,193567,443164,389063,
                        679382,401711,446385,392253,85841,116941,378137,477578,532183,505350,166907,271925,620157,530790,
                        231601,186142,507676,657362,349234,649727,305938,348231,697574,562135,236958,78138,27017,168707,
                        89986,247657,226231,241378,52936,479391,156999,401739,226692,263422,500947,253086,570298,64452,
                        405025,378900,596296,530999,145994,322545,558501,593782,430403,11576,414070,43846,324434,78358,
                        502943,267185,590974,571378,403302,281185,388053,563007,80918,134981,1092,40952,434935,520750,
                        151444,498634,50916,215240,246514,534040,579613,648187,180807,670510,278494,595257,248865,683187,
                        345826,457619,581835,51622,597542,139805,557408,95200,621558,371979,197152,52318,513229,281452,
                        694400,641599,57994,88316,270758,612670,34072,329175,416621,80805,353671,116277,630438,290309,
                        611723,482745,22297,250872,142233,176815,300357,670684,656664,230176,442065,454494,572431,604887,
                        590406,389564,251031,687642,46790,532003,122216,20026,457476,335210,396773,237112,544900,350912,
                        666510,516275,656840,564609,586434,209022,520564,531914,266547,87172,527304,22235,352308,196285,
                        7704,693120,10172,266681,117683,443018,630909,682191,124811,366958,481565,483842,314225,106854,
                        245182,13678,118921,473039,314483,510633,166530,110489,644269,580378,279091,88039,567477,584650,
                        664533,617120,442065,238614,497131,318191,4346,440634,42414,13345,497913,329882,195683,38936,
                        682751,459807,250846,218120,688823,670973,685889,578904,525170,443495,532537,103497,30279,431158,
                        438994,165578,337400,2342,529109,201588,563954,107648,599334,219220,617589,563602,73233,183740,
                        194232,332763,268983,103095,2971,226084,695600,496718,539637,518697,564049,173337,211673,379174,
                        316485,90790,477131,387414,87217,644115,514154,194442,297690,58449,616629,244040,479291,324121,
                        115275,479931,455355,625377,132954,306518,206475,376039,505031,691073,63914,183242,664507,123147,
                        588709,406468,623129,316944,321379,22001,454851,287215,621522,324168,686373,53415,82265,389458,
                        482060,507586,239939,633044,250909,652165,285427,499157,597897,674973,341992,252169,177423,620185,
                        222801,441955,563593,136442,270903,111810,167561,135522,149272,458759,608123,393847,615496,505324,
                        339544,378541,185841,604696,537392,339813,156355,387165,500521,179032,88965,507045,322825,143658,
                        465648,388101,276598,495402,227082,69480,106879,35064,97307,639815,29054,315766,594383,613317,
                        458170,459992,407456,320487,498013,172890,290427,674191,509275,10627,235454,615307,469937,659787,
                        621266,151110,491050,286744,318109,512264,657031,75195,347477,130041,487029,214386,534258,263018,
                        656301,295905,337297,158244,729,487635,684616,253937]

fade :: (Num a) => a -> a
fade t = t ^ 3 * (t * (t * 6 - 15) + 10)

grad :: Integer -> Float -> Float -> Float -> Float
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
-- End of the Perlin noise functions.
--
