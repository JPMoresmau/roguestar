\section{RSAGL.Matrix}

\begin{code}
module RSAGL.Matrix
    (Matrix,
     matrix,
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
     matrixInversePrim)
    where

import Data.List
import RSAGL.Angle
import RSAGL.Vector
\end{code}

The Matrix data structure stores copies of the matrix in both row-major and column-major form,
as well a copy of the Matrix's inverse.  Caching this information has been shown to lead to performance
increases.

In row-major form, each list represents one row; columns run between lists.
In column-major form, each list represents one column; rows run between lists.

In row-major form, the Haskell list representation of a matrix has the same orientation
as if written on paper.

\begin{code}
data Matrix = Matrix { rows, cols :: Int, 
                       row_major :: [[Double]], 
                       matrix_inverse :: Matrix,
                       matrix_determinant :: Double }

instance Eq Matrix where
    x == y = rows x == rows y &&
             cols x == cols y &&
             row_major x == row_major y

instance Show Matrix where
    show m = show $ row_major m

rowMajorForm :: Matrix -> [[Double]]
rowMajorForm mat = row_major mat

colMajorForm :: Matrix -> [[Double]]
colMajorForm = transpose . row_major
\end{code}

rowAt answers the nth row of a matrix.

\begin{code}
rowAt :: Matrix -> Int -> [Double]
rowAt m n = (rowMajorForm m) !! n
\end{code}

matrixAt answers the (i'th,j'th) element of a matrix.

\begin{code}
matrixAt :: Matrix -> (Int,Int) -> Double
matrixAt m (i,j) = ((rowMajorForm m) !! j) !! i
\end{code}

\subsection{Constructing matrices}

matrix constructs a matrix from row major list form.  (Such a list form can be formatted correctly in 
monospaced font and haskell syntax, so that it looks like a matrix as it would be normally written.)

\begin{code}
matrix :: [[Double]] -> Matrix
matrix dats = if all (== row_length) row_lengths
              then m
              else error "row lengths do not match"
    where m = Matrix { rows=length dats, --the number of rows is the length of each column
                       cols=length $ head dats, --the number of columns is the length of each row
                       row_major=dats,
                       matrix_inverse = (matrixInversePrim m) { matrix_inverse = m },
                       matrix_determinant = determinantPrim m }
          row_lengths = map length dats
          row_length = head row_lengths
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
				      then map ((map (uncurry (+))).(uncurry zip)) $ zip (row_major m) (row_major n)
				      else error "matrixAdd: dimension mismatch")
		    in matrix new_row_major
\end{code}

\label{howtoUseMatrixMultiplyImpl}

\begin{code}
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply m n | cols m /= rows n = error "matrixMultiply: dimension mismatch"
matrixMultiply m n = matrix $ [[sum $ zipWith (*) m' n' | n' <- n_data] | m' <- m_data]
    where m_data = row_major m
	  n_data = transpose $ row_major n

matrixTranspose :: Matrix -> Matrix
matrixTranspose = matrix . colMajorForm -- works because matrix expects row major form
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
reduceMatrix m (i,j) =
    let (above,below) = splitAt j $ rowMajorForm m
        (left,right) = splitAt i $ transpose $ above ++ tail below
        in matrix $ transpose $ left ++ tail right
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
matrixInversePrim :: Matrix -> Matrix
matrixInversePrim m | rows m /= cols m = error "matrixInversePrim: not a square matrix"
matrixInversePrim m | determinant m == 0 = error "matrixInversePrim: det m = 0"
matrixInversePrim m = 
    let scale_factor = 1 / determinant m
        in matrixTranspose $ matrix [[scale_factor * matrixCofactor m (i,j) | i <- [0..(cols m-1)]]
                                                                            | j <- [0..(rows m-1)]]

determinantPrim :: Matrix -> Double
determinantPrim m | rows m /= cols m = error "determinantPrim: not a square matrix"
determinantPrim (Matrix { row_major=[[x]] }) = x
determinantPrim m = sum $ zipWith (*) (rowAt m 0) $ map (\x -> matrixCofactor m (x,0)) [0..(cols m - 1)]
\end{code}
