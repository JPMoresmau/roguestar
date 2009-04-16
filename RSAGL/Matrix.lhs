\section{RSAGL.Matrix}

\begin{code}

module RSAGL.Matrix
    (Matrix,
     matrix,
     columnMatrix4,
     unsafeFromRowMatrix3,
     rowMajorForm,
     colMajorForm,
     rowAt,
     matrixAt,
     identityMatrix,
     translationMatrix,
     rotationMatrix,
     scaleMatrix,
     xyzMatrix,
     matrixAdd,
     matrixMultiply,
     matrixTranspose,
     matrixInverse,
     determinant,
     matrixInversePrim,
     matrixTransposePrim,
     matrixInverseTransposePrim)
    where

import Data.List
import RSAGL.Angle
import RSAGL.Vector
import Data.Array.ST
import Data.Array.Base
\end{code}

The Matrix data structure stores copies of the matrix in both row-major and column-major form,
as well a copy of the Matrix's inverse.  Caching this information has been shown to lead to performance
increases.

In row-major form, each list represents one row; columns run between lists.
In column-major form, each list represents one column; rows run between lists.

In row-major form, the Haskell list representation of a matrix has the same orientation
as if written on paper.

\begin{code}
data Matrix = Matrix { matrix_rows, matrix_cols :: !Int, 
                       matrix_data :: UArray Int Double, 
                       matrix_inverse :: Matrix,
                       matrix_transpose :: Matrix,
                       matrix_determinant :: Double }

{-# INLINE rows #-}
rows :: Matrix -> Int
rows = matrix_rows

{-# INLINE cols #-}
cols :: Matrix -> Int
cols = matrix_cols

instance Eq Matrix where
    x == y = rows x == rows y &&
             cols x == cols y &&
             rowMajorForm x == rowMajorForm y

instance Show Matrix where
    show m = show $ rowMajorForm m

rowMajorForm :: Matrix -> [[Double]]
rowMajorForm mat = unfoldr (\xs -> if null xs then Nothing else Just $ splitAt (matrix_cols mat) xs) $ elems $ matrix_data mat

colMajorForm :: Matrix -> [[Double]]
colMajorForm = transpose . rowMajorForm
\end{code}

rowAt answers the nth row of a matrix.

\begin{code}
rowAt :: Matrix -> Int -> [Double]
rowAt m n = (rowMajorForm m) !! n
\end{code}

matrixAt answers the (i'th,j'th) element of a matrix.

\begin{code}
matrixAt :: Matrix -> (Int,Int) -> Double
matrixAt m (i,j) = case () of
    () | i `seq` j `seq` False -> undefined
    () | i >= rows m || j >= cols m -> error "matrixAt: out of bounds"
    () | otherwise -> uncheckedMatrixAt m (i,j)

{-# INLINE uncheckedMatrixAt #-}
uncheckedMatrixAt :: Matrix -> (Int,Int) -> Double
uncheckedMatrixAt m (i,j) = case () of
    () | i `seq` j `seq` False -> undefined
    () | otherwise -> matrix_data m `unsafeAt` ((i*cols m) + j)
\end{code}

\subsection{Constructing matrices}

matrix constructs a matrix from row major list form.  (Such a list form can be formatted correctly in 
monospaced font and haskell syntax, so that it looks like a matrix as it would be normally written.)

\begin{code}
matrix :: [[Double]] -> Matrix
matrix [] = error "matrix: empty matrix"
matrix [[]] = error "matrix: empty matrix"
matrix dats | not (all (== length (head dats)) (map length dats)) = error "matrix: row lengths do not match"
matrix dats = uncheckedMatrix number_of_rows number_of_cols (listArray (0,number_of_rows * number_of_cols - 1) $ concat dats)
    where number_of_rows = length dats
          number_of_cols = length $ head dats

-- | Generate a column matrix of length 4.
{-# INLINE columnMatrix4 #-}
columnMatrix4 :: Double -> Double -> Double -> Double -> Matrix
columnMatrix4 x y z w = seq x $ seq y $ seq z $ seq w $ uncheckedMatrix 4 1 $ runSTUArray $
    do a <- newArray_ (0,3)
       unsafeWrite a 0 x
       unsafeWrite a 1 y
       unsafeWrite a 2 z
       unsafeWrite a 3 w
       return a

-- | Generate a point or vector value from a row matrix of length (at least) 3.
{-# INLINE unsafeFromRowMatrix3 #-}
unsafeFromRowMatrix3 :: (Double -> Double -> Double -> a) -> Matrix -> a
unsafeFromRowMatrix3 f m = f (uncheckedMatrixAt m (0,0)) (uncheckedMatrixAt m (0,1)) (uncheckedMatrixAt m (0,2))
    
uncheckedMatrix :: Int -> Int -> UArray Int Double -> Matrix
uncheckedMatrix number_of_rows number_of_cols dats = m
    where m_inverse = matrixTransposePrim m_inverse_transpose { matrix_inverse = m, matrix_transpose = m_inverse_transpose, matrix_determinant = recip m_det }
          m_transpose = matrixTransposePrim m { matrix_inverse = m_inverse_transpose, matrix_transpose = m, matrix_determinant = m_det }
          m_inverse_transpose = matrixInverseTransposePrim m { matrix_inverse = m_transpose, matrix_transpose = m_inverse, matrix_determinant = recip m_det }
          m_det = determinantPrim m
          m = Matrix { matrix_rows=number_of_rows,
                       matrix_cols=number_of_cols,
                       matrix_data=dats,
                       matrix_inverse = m_inverse,
                       matrix_transpose = m_transpose,
                       matrix_determinant = m_det }
\end{code}

identityMatrix constructs the n by n identity matrix

\begin{code}
identityMatrix :: (Integral i) => i -> Matrix
identityMatrix n = matrix $ map (\x -> genericReplicate x 0 ++ [1] ++ genericReplicate (n-1-x) 0) [0..n-1]
\end{code}

\begin{code}
translationMatrix :: Vector3D -> Matrix
translationMatrix (Vector3D x y z) = matrix [[1,0,0,x],
					     [0,1,0,y],
					     [0,0,1,z],
					     [0,0,0,1]]
\end{code}

\begin{code}
rotationMatrix :: Vector3D -> Angle -> Matrix
rotationMatrix vector angle = let s = sine angle
				  c = cosine angle
				  c' = 1 - c
				  (Vector3D x y z) = vectorNormalize vector
				  in matrix [[c+c'*x*x,     c'*y*x - s*z,   c'*z*x + s*y, 0],
					     [c'*x*y+s*z,   c+c'*y*y,       c'*z*y - s*x, 0],
					     [c'*x*z-s*y,   c'*y*z+s*x,     c+c'*z*z,     0],
					     [0,            0,              0,            1]]
\end{code}

\begin{code}
scaleMatrix :: Vector3D -> Matrix
scaleMatrix (Vector3D x y z) = matrix [[x, 0, 0, 0],
				       [0, y, 0, 0],
				       [0, 0, z, 0],
				       [0, 0, 0, 1]]
\end{code}

\texttt{xyzMatrix} constructs the matrix in which the x y and z axis are transformed to point in the direction of the specified
vectors.

\begin{code}
xyzMatrix :: Vector3D -> Vector3D -> Vector3D -> Matrix
xyzMatrix (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) (Vector3D x3 y3 z3) =
    matrix [[x1,x2,x3,0],
            [y1,y2,y3,0],
            [z1,z2,z3,0],
            [0,0,0,1.0]]
\end{code}

\subsection{Matrix arithmetic}

\begin{code}
matrixAdd :: Matrix -> Matrix -> Matrix
matrixAdd m n = let new_row_major = (if and [rows m == rows n,cols m == cols n]
				     then map ((map (uncurry (+))).(uncurry zip)) $ zip (rowMajorForm m) (rowMajorForm n)
				     else error "matrixAdd: dimension mismatch")
		    in matrix new_row_major
\end{code}

\label{howtoUseMatrixMultiplyImpl}

\begin{code}
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply m n | cols m /= rows n = error "matrixMultiply: dimension mismatch"
matrixMultiply m n = case () of
        () | number_of_cols `seq` number_of_rows `seq` run_length `seq` False -> undefined
	() | otherwise -> uncheckedMatrix number_of_rows number_of_cols new_data
    where number_of_cols = cols n
          number_of_rows = rows m
	  run_length = cols m
          loop a z f = case () of
	      () | a `seq` z `seq` False -> undefined
	      () | a < z -> do f a
	                       loop (a+1) z f
	      () | otherwise -> return ()
	  multiplyCell i j r = case () of
	      () | i `seq` j `seq` r `seq` False -> undefined
	      () | r < run_length -> uncheckedMatrixAt m (i,r) * uncheckedMatrixAt n (r,j) + multiplyCell i j (r+1)
	      () | otherwise -> 0
          new_data = runSTUArray $
              do a <- newArray_ (0,number_of_rows * number_of_cols - 1)
		 loop 0 number_of_rows $ \this_row -> 
		     let this_row_start = this_row*number_of_cols
		         in seq this_row_start $ loop 0 number_of_cols $ \this_col -> 
		                unsafeWrite a (this_row_start+this_col) (multiplyCell this_row this_col 0)
		 return a

matrixTranspose :: Matrix -> Matrix
matrixTranspose = matrix_transpose
\end{code}

\subsection{Inverse and determinant of a matrix}

\begin{code}
matrixInverse :: Matrix -> Matrix
matrixInverse = matrix_inverse
\end{code}

\begin{code}
determinant :: Matrix -> Double
determinant = matrix_determinant
\end{code}

reduceMatrix eliminates the row and column corresponding to a specific element of a matrix.

\begin{code}
reduceMatrix :: Matrix -> (Int,Int) -> Matrix
reduceMatrix m (i,j) = case () of
    () | num_rows `seq` num_cols `seq` i `seq` j `seq` False -> undefined
    () | i >= num_rows || j >= num_cols -> error "reduceMatrix: out of bounds"
    () | otherwise -> uncheckedMatrix (rows m - 1) (cols m - 1) $ ixmap (0,rows m * cols m - rows m - cols m) (\x ->
        let n = (x + j_push) `div` new_cols
	    p = if x `div` new_cols >= i then num_cols else 0
            in case () of
	        () | x `seq` n `seq` p `seq` False -> undefined
	        () | otherwise -> x + n + p) $ matrix_data m
    where num_rows = rows m
          num_cols = cols m
	  new_cols = num_cols - 1
	  j_push = num_cols - j - 1
\end{code}

The minor of an element of a matrix is the determinant of the matrix that is formed by removing
the row and column corresponding to that element (see reduceMatrix).

\begin{code}
matrixMinor :: Matrix -> (Int,Int) -> Double
matrixMinor m ij = determinant $ reduceMatrix m ij
\end{code}

The cofactor of m at (i,j) is the minor of m at (i,j), multiplied by -1 at checkerboarded elements.

\begin{code}
matrixCofactor :: Matrix -> (Int,Int) -> Double
matrixCofactor m (0,0) | rows m == 1 && cols m == 1 = matrixAt m (0,0)
matrixCofactor m (i,j) = (-1)^(i+j) * matrixMinor m (i,j)
\end{code}

Implementation of determinant and matrix inverse for matrices of Rationals.

\begin{code}
matrixInverseTransposePrim :: Matrix -> Matrix
matrixInverseTransposePrim m | rows m /= cols m = error "matrixInverseTransposePrim: not a square matrix"
matrixInverseTransposePrim m | determinant m == 0 = error "matrixInverseTransposePrim: det m = 0"
matrixInverseTransposePrim m = 
    let scale_factor = 1 / determinant m
        in matrix [[scale_factor * matrixCofactor m (i,j) | j <- [0..(cols m-1)]]
                                                          | i <- [0..(rows m-1)]]

matrixInversePrim :: Matrix -> Matrix
matrixInversePrim = matrixTransposePrim . matrixInverseTransposePrim

determinantPrim :: Matrix -> Double
determinantPrim m | rows m /= cols m = error "determinantPrim: not a square matrix"
determinantPrim m | rows m == 1 && cols m == 1 = matrixAt m (0,0)
determinantPrim m | rows m == 2 && cols m == 2 = uncheckedMatrixAt m (0,0) * uncheckedMatrixAt m (1,1) -
                                                 uncheckedMatrixAt m (1,0) * uncheckedMatrixAt m (0,1)
determinantPrim m | rows m == 3 && cols m == 3 = uncheckedMatrixAt m (0,0) * uncheckedMatrixAt m (1,1) * uncheckedMatrixAt m (2,2) +
                                                 uncheckedMatrixAt m (0,1) * uncheckedMatrixAt m (1,2) * uncheckedMatrixAt m (2,0) +
						 uncheckedMatrixAt m (0,2) * uncheckedMatrixAt m (1,0) * uncheckedMatrixAt m (2,1) -
						 uncheckedMatrixAt m (2,0) * uncheckedMatrixAt m (1,1) * uncheckedMatrixAt m (0,2) -
						 uncheckedMatrixAt m (2,1) * uncheckedMatrixAt m (1,2) * uncheckedMatrixAt m (0,0) -
						 uncheckedMatrixAt m (2,2) * uncheckedMatrixAt m (1,0) * uncheckedMatrixAt m (0,1)
determinantPrim m = sum $ zipWith (*) (rowAt m 0) $ map (\x -> matrixCofactor m (0,x)) [0..(cols m - 1)]

matrixTransposePrim :: Matrix -> Matrix
matrixTransposePrim = matrix . colMajorForm -- works because matrix expects row major form
\end{code}
