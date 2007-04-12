\section{RSAGL.Matrix}

\begin{code}
module RSAGL.Matrix
    (Matrix,
     matrix,
     rowAt,
     matrixAt,
     coerceMatrix,
     identityMatrix,
     xyzMatrix,
     matrixAdd,
     matrixMultiply,
     matrixTranspose,
     matrixInverse,
     determinant,
     MatrixType(..))
    where

import Data.List
import Data.Array
import Data.Maybe
import Data.Ratio
import RSAGL.Angle
import RSAGL.Vector
import Control.Monad
\end{code}

The Matrix data structure stores copies of the matrix in both row-major and column-major form,
as well a copy of the Matrix's inverse.  Caching this information has been shown to lead to performance
increases.

In row-major form, each list represents one row; columns run between lists.
In column-major form, each list represents one column; rows run between lists.

In row-major form, the Haskell list representation of a matrix has the same orientation
as if written on paper.

\begin{code}
data Matrix a = Matrix { rows, cols :: Integer, row_major, col_major :: [[a]], matrix_inverse :: Matrix a }

instance (Eq a) => Eq (Matrix a) where
    x == y = rows x == rows y &&
             cols x == cols y &&
             row_major x == row_major y

instance (Show a) => Show (Matrix a) where
    show m = show $ row_major m

rowMajorForm :: Matrix a -> [[a]]
rowMajorForm mat = row_major mat

colMajorForm :: Matrix a -> [[a]]
colMajorForm mat = col_major mat
\end{code}

\begin{code}
coerceMatrix :: (MatrixType b) => (a -> b) -> Matrix a -> Matrix b
coerceMatrix fn m = matrix $ map (map fn) $ rowMajorForm m
\end{code}

rowAt answers the nth row of a matrix.

\begin{code}
rowAt :: Matrix a -> Integer -> [a]
rowAt m n = (rowMajorForm m) `genericIndex` n
\end{code}

matrixAt answers the (i'th,j'th) element of a matrix.

\begin{code}
matrixAt :: Matrix a -> (Integer,Integer) -> a
matrixAt m (i,j) = ((rowMajorForm m) `genericIndex` j) `genericIndex` i
\end{code}

\subsection{Constructing matrices}

matrix constructs a matrix from row major list form.  (Such a list form can be formatted correctly in 
monospaced font and haskell syntax, so that it looks like a matrix as it would be normally written.)

\begin{code}
matrix :: (MatrixType a) => [[a]] -> Matrix a
matrix dats = let row_lengths = map genericLength dats
		  row_length = head row_lengths
		  in (if all (== row_length) row_lengths
		      then m
		      else error "row lengths do not match")
    where m = Matrix { rows=genericLength dats, --the number of rows is the length of each column
                       cols=genericLength $ head dats, --the number of columns is the length of each row
                       row_major=dats,
                       col_major=transpose dats,
                       matrix_inverse = (matrixInversePrim m) { matrix_inverse = m } }
\end{code}

identityMatrix constructs the n by n identity matrix

\begin{code}
identityMatrix :: (MatrixType a,Num a,Integral i) => i -> Matrix a
identityMatrix n = matrix $ map (\x -> genericReplicate x 0 ++ [1] ++ genericReplicate (n-1-x) 0) [0..n-1]
\end{code}

\begin{code}
translationMatrix :: Vector3D -> Matrix Double
translationMatrix (Vector3D x y z) = matrix [[1,0,0,x],
					     [0,1,0,y],
					     [0,0,1,z],
					     [0,0,0,1]]
\end{code}

\begin{code}
rotationMatrix :: Vector3D -> Angle -> Matrix Double
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
scaleMatrix :: Vector3D -> Matrix Double
scaleMatrix (Vector3D x y z) = matrix [[x, 0, 0, 0],
				       [0, y, 0, 0],
				       [0, 0, z, 0],
				       [0, 0, 0, 1]]
\end{code}

xyzMatrix constructs the matrix in which the x y and z axis are transformed to point in the direction of the specified
vectors.

\begin{code}
xyzMatrix :: Vector3D -> Vector3D -> Vector3D -> Matrix Double
xyzMatrix (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) (Vector3D x3 y3 z3) =
    matrix [[x1,x2,x3,0],
            [y1,y2,y3,0],
            [z1,z2,z3,0],
            [0,0,0,1.0]]
\end{code}

\subsection{Matrix arithmetic}

\begin{code}
matrixAdd :: (MatrixType a,Num a) => Matrix a -> Matrix a -> Matrix a
matrixAdd m n = let new_row_major = (if and [rows m == rows n,cols m == cols n]
				      then map ((map (uncurry (+))).(uncurry zip)) $ zip (row_major m) (row_major n)
				      else error "matrixAdd: dimension mismatch")
		    in matrix new_row_major

matrixMultiply :: (MatrixType a,Num a) => Matrix a -> Matrix a -> Matrix a
matrixMultiply m n | cols m /= rows n = error "matrixMultiply: dimension mismatch"
matrixMultiply m n = let m_data = row_major m
			 n_data = col_major n
			 new_row_major = [[sum $ zipWith (*) m' n' | n' <- n_data] | m' <- m_data] 
			 in matrix new_row_major

matrixTranspose :: (MatrixType a) => Matrix a -> Matrix a
matrixTranspose = matrix . colMajorForm -- works because matrix expects row major form
\end{code}

\subsection{Inverse and determinant of a matrix}      

\begin{code}
matrixInverse :: (Num a,Fractional a,Real a) => Matrix a -> Matrix a
matrixInverse = matrix_inverse
\end{code}

\begin{code}
determinant :: (MatrixType a,Num a) => Matrix a -> a
determinant m | rows m /= cols m = error "determinant: not a square matrix"
determinant (Matrix { row_major=[[x]] }) = x
determinant m = sum $ zipWith (*) (rowAt m 0) $ map (\x -> matrixCofactor m (x,0)) [0..(cols m - 1)]
\end{code}

reduceMatrix eliminates the row and column corresponding to a specific element of a matrix.

\begin{code}
reduceMatrix :: (MatrixType a,Num a) => Matrix a -> (Integer,Integer) -> Matrix a
reduceMatrix m (i,j) =
    let (above,below) = genericSplitAt j $ rowMajorForm m
        (left,right) = genericSplitAt i $ transpose $ above ++ tail below
        in matrix $ transpose $ left ++ tail right
\end{code}

The minor of an element of a matrix is the determinant of the matrix that is formed by removing
the row and column corresponding to that element (see reduceMatrix).

\begin{code}
matrixMinor :: (MatrixType a,Num a) => Matrix a -> (Integer,Integer) -> a
matrixMinor m ij = determinant $ reduceMatrix m ij
\end{code}

The cofactor of m at (i,j) is the minor of m at (i,j), multiplied by -1 at checkerboarded elements.

\begin{code}
matrixCofactor :: (MatrixType a,Num a) => Matrix a -> (Integer,Integer) -> a
matrixCofactor m (0,0) | rows m == 1 && cols m == 1 = matrixAt m (0,0)
matrixCofactor m (i,j) = (-1)^(i+j) * matrixMinor m (i,j)
\end{code}

Implementation of matrix inverse for matrices of Rationals.

\begin{code}
matrixInverseFractional :: (MatrixType a,Num a,Fractional a) => Matrix a -> Matrix a
matrixInverseFractional m | rows m /= cols m = error "matrixInverseRational: not a square matrix"
matrixInverseFractional m | determinant m == 0 = error "matrixInverseRational: det m = 0"
matrixInverseFractional m = 
    let scale_factor = 1 / determinant m
        in matrixTranspose $ matrix [[scale_factor * matrixCofactor m (i,j) | i <- [0..(cols m-1)]]
                                                                            | j <- [0..(rows m-1)]]
\end{code}

\subsection{The MatrixType typeclass}

The MatrixType typeclass defines how the inverse of a matrix is taken.

\begin{code}
class MatrixType a where
    matrixInversePrim :: Matrix a -> Matrix a

instance MatrixType Double where
    matrixInversePrim = matrixInverseFractional
    
instance MatrixType Float where
    matrixInversePrim = matrixInverseFractional
    
instance (Integral a) => MatrixType (Ratio a) where
    matrixInversePrim = matrixInverseFractional
\end{code}
