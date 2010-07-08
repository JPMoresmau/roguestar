\section{Optimization of Rendered Surfaces}

\begin{code}
module RSAGL.Modeling.Optimization
    (optimizeSurface,
     allocateComplexity,
     estimateSurfaceArea)
    where

import RSAGL.Auxiliary.Auxiliary
import RSAGL.Math.Curve
import Data.List as List
import RSAGL.Modeling.Tesselation
import RSAGL.Math.Types
\end{code}

\subsection{Surface Configurations}

A \texttt{SurfaceConfiguration} represents an arrangement of vertices over a surface, with the goal that the arrangement will be made optimally.

For example, the parametric equation of a sphere maps two-dimensional points making up a 1-by-1 square onto the three-dimensional sphere. If points are arranged evenly over the square, then those points will be concentrted tightly around the poles and loosely around the equator.

\texttt{SurfaceConfiguration} is the data structure that we try to optimize to get the ideal arrangement of points.
\begin{code}
data SurfaceConfiguration = SurfaceConfiguration [Integer]
    deriving (Eq,Ord,Show)
\end{code}

\subsection{Choosing an optimal level of detail}

The goal of \texttt{optimizeSurface} is to allocate points so they are spread roughly evenly over a surface.

To measure the distance between points, we use an abstract function called the ''ruler``, which nominally measures pythagorean distance but may measure some kind of percieved distance.

\begin{code}
optimizeSurface :: (a -> a -> RSdouble) -> Surface a -> Integer -> TesselatedSurface a
optimizeSurface ruler s max_vertices =
              if score ordinary >= score transposed
              then tesselateSurfaceConfiguration s ordinary
              else tesselateSurfaceConfiguration (flipTransposeSurface s) transposed
    where ordinary = lengthProportional ruler s max_vertices
          transposed = lengthProportional ruler (flipTransposeSurface s) max_vertices
          score :: SurfaceConfiguration -> RSdouble
          score (SurfaceConfiguration ielems) = 
              let elems = map fromIntegral ielems
                  in (sum (map (abs . subtract (sum elems / fromIntegral (length elems))) elems) + 1) / sum elems

tesselateSurfaceConfiguration :: Surface a -> SurfaceConfiguration -> TesselatedSurface a
tesselateSurfaceConfiguration s (SurfaceConfiguration elems) = 
        tesselateGrid $ map zto $ zipWith iterateCurve elems $
                                           halfIterateSurface (genericLength elems) s
    where zto xs = zip (zeroToOne $ genericLength xs) xs

\end{code}

\subsection{Estimating Surface Area and Curve Length}

\begin{code}
estimateCurveLength :: (a -> a -> RSdouble) -> Curve a -> RSdouble
estimateCurveLength ruler c = case sum $ map (uncurry ruler) $ doubles $ iterateCurve 16 c of -- 16 is arbitrary
    x | isNaN x || isInfinite x -> error "estimateCurveLength: NaN"
    x -> x

estimateSurfaceArea :: (a -> a -> RSdouble) -> Surface a -> RSdouble
estimateSurfaceArea ruler s = snd $ head $ dropWhile (\(x,y) -> y > x*1.125) $ doubles surface_areas_at_increasing_levels_of_detail
    where surface_areas_at_increasing_levels_of_detail = map (\d -> estimateTesselatedSurfaceArea ruler $ tesselateSurface s (d,d)) $ iterate (*2) 4

estimateTesselatedSurfaceArea :: (a -> a -> RSdouble) -> TesselatedSurface a -> RSdouble
estimateTesselatedSurfaceArea ruler pieces = sum $ map measurePiece pieces
   where measurePiece (TesselatedTriangleFan (v0:v1:v2:vs)) = heronsFormula ruler v0 v1 v2 +
                                                              measurePiece (TesselatedTriangleFan (v0:v2:vs))
         measurePiece (TesselatedTriangleFan _) = 0.0
         measurePiece (TesselatedTriangleStrip (v0:v1:v2:vs)) = heronsFormula ruler v0 v1 v2 +
                                                              measurePiece (TesselatedTriangleStrip (v1:v2:vs))
         measurePiece (TesselatedTriangleStrip _) = 0.0
         measurePiece (TesselatedTriangles (v0:v1:v2:vs)) = heronsFormula ruler v0 v1 v2 +
                                                            measurePiece (TesselatedTriangles vs)
         measurePiece (TesselatedTriangles _) = 0.0

heronsFormula :: (a -> a -> RSdouble) -> a -> a -> a -> RSdouble
heronsFormula ruler v0 v1 v2 = max 0 $ (/ 4) $ sqrt $ (a + (b + c)) * (c - (a - b)) * (c + (a - b)) * (a + (b - c))
    where a_ = ruler v0 v1
          b_ = ruler v1 v2
          c_ = ruler v2 v0
          [c,b,a] = sort [a_,b_,c_]

\end{code}

\subsection{Allocating Proportional Model Complexity}

Estimate the relative size of a collection of surfaces and allocate complexity to those surfaces proportionally.  This function does not bother accounting for rounding errors, so the sum of the complexity counts allocated to the surfaces may not equal the complexity count passed in as a parameter.

\texttt{allocatedComplexity} allocates vertices to surfaces, with half of all vertices being distributed equally among all surfaces and half of all vertices being distributed in proportion to their surface areas.  Surfaces (such as surfaces that have many layers) may be weighted to carry disproportionately more vertices.

\begin{code}
allocateComplexity :: (p -> p -> RSdouble) -> [(Surface p,RSdouble)] -> Integer -> [Integer]
allocateComplexity ruler surfaces n = 
    let surface_areas = map (\s -> estimateSurfaceArea ruler (fst s) * snd s) surfaces
        half_alloc = n `div` 2
        constant_alloc = half_alloc `div` genericLength surfaces
        in map ((+ constant_alloc) . round) $ proportional (fromInteger half_alloc) surface_areas

lengthProportional :: (p -> p -> RSdouble) -> Surface p -> Integer -> SurfaceConfiguration
lengthProportional ruler s n =
    let curve_lengths = map (estimateCurveLength ruler) $ halfIterateSurface base_width s
        transpose_lengths = map (estimateCurveLength ruler) $ halfIterateSurface base_width $ flipTransposeSurface s
        base_width = max 5 $ floor $ sqrt $ fromInteger n
        improved_width = max 5 $ round $ sqrt (sum transpose_lengths / sum curve_lengths) * fromInteger base_width
        roundOdd x = case floor x of
                          x' | even x' -> x' + 1
                          x' -> x'
        in SurfaceConfiguration $ map (max 5 . roundOdd) $ proportional (fromInteger n) $ 
               map (estimateCurveLength ruler) $ halfIterateSurface improved_width s

proportional :: RSdouble -> [RSdouble] -> [RSdouble]
proportional total xs = map (* (total / sum xs)) xs
\end{code}
