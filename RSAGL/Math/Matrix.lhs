\section{RSAGL.Matrix}

\begin{code}

module RSAGL.Math.Matrix
    (Matrix,
     matrix,
     transformHomogenous,
     rowMajorForm,
     colMajorForm,
     rowAt,
     matrixAt,
     identity_matrix,
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
     matrixInverseTransposePrim,
     determinantPrim)
    where

import Data.List as List
import RSAGL.Math.Angle
import RSAGL.Math.Vector
import Data.Vec as Vec
\end{code}

A 4-by-4 matrix with cached inverse, transpose, inverse transpose, and determinant.

\begin{code}
data Matrix = Matrix { matrix_data :: !(Mat44 Double),
                       matrix_inverse :: Matrix,
                       matrix_transpose :: Matrix,
                       matrix_determinant :: Double }

instance Eq Matrix where
    x == y = matrix_data x == matrix_data y

instance Show Matrix where
    show m = show $ rowMajorForm m

rowMajorForm :: Matrix -> [[Double]]
rowMajorForm = matToLists . matrix_data

colMajorForm :: Matrix -> [[Double]]
colMajorForm = List.transpose . rowMajorForm
\end{code}

rowAt answers the nth row of a matrix.

\begin{code}
rowAt :: Matrix -> Int -> [Double]
rowAt m n = (rowMajorForm m) !! n
\end{code}

matrixAt answers the (i'th,j'th) element of a matrix.

\begin{code}
matrixAt :: Matrix -> (Int,Int) -> Double
matrixAt m (i,j) = rowAt m i !! j
\end{code}

\subsection{Constructing matrices}

matrix constructs a matrix from row major list form.  (Such a list form can be formatted correctly in 
monospaced font and haskell syntax, so that it looks like a matrix as it would be normally written.)

\begin{code}
matrix :: [[Double]] -> Matrix
matrix = uncheckedMatrix . matFromLists

-- | Generate a column matrix of length 4, perform an affine transformation on it, and produce the resulting value.
{-# INLINE transformHomogenous #-}
transformHomogenous :: Double -> Double -> Double -> Double -> (Double -> Double -> Double -> a) -> Matrix -> a
transformHomogenous x y z w f m = f x' y' z'
    where (x':.y':.z':._) = multmv (matrix_data m) (x:.y:.z:.w:.())

uncheckedMatrix :: Mat44 Double -> Matrix
uncheckedMatrix dats = m
    where m_inverse = (matrixTransposePrim m_inverse_transpose) { matrix_inverse = m, matrix_transpose = m_inverse_transpose, matrix_determinant = recip m_det }
          m_transpose = (matrixTransposePrim m) { matrix_inverse = m_inverse_transpose, matrix_transpose = m, matrix_determinant = m_det }
          m_inverse_transpose = (matrixInverseTransposePrim m) { matrix_inverse = m_transpose, matrix_transpose = m_inverse, matrix_determinant = recip m_det }
          m_det = determinantPrim m
          m = Matrix { matrix_data=dats,
                       matrix_inverse = m_inverse,
                       matrix_transpose = m_transpose,
                       matrix_determinant = m_det }
\end{code}

identityMatrix constructs the n by n identity matrix

\begin{code}
identity_matrix :: Matrix
identity_matrix = Matrix {
    matrix_data = identity,
    matrix_inverse = identity_matrix,
    matrix_transpose = identity_matrix,
    matrix_determinant = 1 }
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
matrixAdd m n = uncheckedMatrix $ Vec.zipWith (+) (matrix_data m) (matrix_data n)
\end{code}

\label{howtoUseMatrixMultiplyImpl}

\begin{code}
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply m n = uncheckedMatrix $ multmm (matrix_data m) (matrix_data n)

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

\begin{code}
matrixInverseTransposePrim :: Matrix -> Matrix
matrixInverseTransposePrim = uncheckedMatrix . Vec.transpose . fst . invertAndDet . matrix_data

matrixInversePrim :: Matrix -> Matrix
matrixInversePrim = uncheckedMatrix . fst . invertAndDet . matrix_data

determinantPrim :: Matrix -> Double
determinantPrim = det . matrix_data

matrixTransposePrim :: Matrix -> Matrix
matrixTransposePrim = uncheckedMatrix . Vec.transpose . matrix_data
\end{code}
