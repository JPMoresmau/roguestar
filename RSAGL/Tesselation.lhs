\section{Decomposing Surface Data}

\begin{code}
module RSAGL.Tesselation
    (TesselatedSurface,
     TesselatedElement(..),
     tesselatedSurfaceToVertexCloud,
     tesselateSurface,
     tesselateGrid,
     tesselatedElementToOpenGL,
     unmapTesselatedElement)
    where

import RSAGL.Curve
import RSAGL.Auxiliary
import RSAGL.Affine
import RSAGL.BoundingBox
import Data.List
import Control.Parallel.Strategies hiding (r0)
import Control.Arrow
import Graphics.Rendering.OpenGL.GL.BeginEnd
import RSAGL.OpenGLPrimitives
\end{code}

Tesselation is a stage of transforming a model into OpenGL procedure calls.  Tesselation is done by breaking a surface into a sequence of polylines (a grid).  Pairs of polylines, possibly of differing length, describe a polygon strip.  We subdivide that strip into triangle fans and quadralateral strips, as described by the OpenGL specification.

\begin{code}
type TesselatedSurface a = [TesselatedElement a]

data TesselatedElement a = TesselatedTriangleFan [a]
    deriving (Read,Show)

instance (AffineTransformable a) => AffineTransformable (TesselatedElement a) where
    transform m (TesselatedTriangleFan as) = TesselatedTriangleFan $ transform m as

instance (NFData a) => NFData (TesselatedElement a) where
    rnf (TesselatedTriangleFan as) = rnf as

instance Functor TesselatedElement where
    fmap f (TesselatedTriangleFan as) = TesselatedTriangleFan $ fmap f as

tesselatedSurfaceToVertexCloud :: TesselatedSurface a -> [a]
tesselatedSurfaceToVertexCloud = concatMap $ \(TesselatedTriangleFan as) -> as

instance (Bound3D a) => Bound3D (TesselatedElement a) where
    boundingBox x = boundingBox $ tesselatedSurfaceToVertexCloud [x]

tesselateSurface :: Surface a -> (Integer,Integer) -> TesselatedSurface a
tesselateSurface s uv = tesselateGrid $ iterateSurface uv (zipSurface (,) (fmap fst uv_identity) s)

tesselateGrid :: [[(Double,a)]] -> TesselatedSurface a
tesselateGrid = concatMap (uncurry tesselateStrip) . doubles

tesselateStrip :: [(Double,a)] -> [(Double,a)] -> TesselatedSurface a
tesselateStrip lefts rights = tesselatePieces $ tesselateSteps lefts rights

tesselateSteps :: [(Double,a)] -> [(Double,a)] -> [Either a a]
tesselateSteps lefts rights = map (either (Left . snd) (Right . snd)) $ sortBy (\x y -> compare (either fst fst x) (either fst fst y)) 
                                      (map Left (tesselateResequence lefts) ++ map Right (tesselateResequence rights))

tesselateResequence :: [(Double,a)] -> [(Double,a)]
tesselateResequence [] = []
tesselateResequence [a] = [a]
tesselateResequence (a:as) = a : map (\((x,_),(y,b)) -> ((x+y)/2,b)) aas
    where aas = doubles (a:as)

tesselatePieces :: [Either a a] -> TesselatedSurface a
tesselatePieces [] = []
tesselatePieces [_] = []
tesselatePieces [_,_] = []
tesselatePieces xs = fst best : tesselatePieces (snd best)
    where rightside_triangle = tesselateAsRightSidedTriangle xs
          leftside_triangle = tesselateAsLeftSidedTriangle xs
          best = case (length $ fst leftside_triangle,length $ fst rightside_triangle) of
                     (0,0) -> (undefined,[])
                     (l,r) | l >= r -> first TesselatedTriangleFan leftside_triangle
                     _ -> first TesselatedTriangleFan rightside_triangle

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = either (const False) (const True)

stripEither :: Either a a -> a
stripEither = either id id

tesselateAsLeftSidedTriangle :: [Either a a] -> ([a],[Either a a])
tesselateAsLeftSidedTriangle = tesselateAsSidedTriangle isLeft

tesselateAsRightSidedTriangle :: [Either a a] -> ([a],[Either a a])
tesselateAsRightSidedTriangle = first (\x -> take 1 x ++ reverse (drop 1 x)) . tesselateAsSidedTriangle isRight

tesselateAsSidedTriangle :: (Either a a -> Bool) -> [Either a a] -> ([a],[Either a a])
tesselateAsSidedTriangle test lrs =    -- looking for a pattern that contains at least two Lefts and exactly one Right 
                                       -- (except the test may be reversed Left for Right)
        if ok then (map stripEither $ right_vertex : (leading_lefts ++ trailing_lefts),
	                               -- the triangle strip defined by one right edge and several left edges
                    right_vertex : (last $ leading_lefts ++ trailing_lefts) : rest)      
		                       -- the right edge and the trailing left edge define the start of the next strip
              else ([],lrs)            -- pattern match failure, return the parameters as we recieved them to pattern match on something else
    where (leading_lefts,trailing) = span test lrs
          right_vertex = head trailing
          (trailing_lefts,rest) = span test $ tail trailing
          ok = (not $ null trailing) && ((not $ null leading_lefts) || (not $ null trailing_lefts))
\end{code}

\subsection{Sending decomposed data to OpenGL}

\begin{code}
tesselatedElementToOpenGL :: (OpenGLPrimitive a) => Bool -> TesselatedElement a -> IO ()
tesselatedElementToOpenGL colors_on (TesselatedTriangleFan xs) = renderPrimitives TriangleFan colors_on xs

unmapTesselatedElement :: TesselatedElement a -> (PrimitiveMode,[a])
unmapTesselatedElement (TesselatedTriangleFan as) = (TriangleFan,as)
\end{code}
