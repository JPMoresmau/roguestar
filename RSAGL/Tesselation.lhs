\section{Decomposing Surface Data}

\begin{code}
module RSAGL.Tesselation
    (TesselatedSurface,
     TesselatedElement(..),
     tesselateSurface,
     tesselateGrid,
     tesselatedElementToOpenGL,
     ConcavityDetection(..))
    where

import RSAGL.Vector
import RSAGL.Auxiliary
import RSAGL.Surface
import RSAGL.Affine
import Data.List
import Data.DeepSeq
import Control.Arrow
import Graphics.Rendering.OpenGL.GL.BeginEnd
\end{code}

Tesselation is a stage of transforming a model into OpenGL procedure calls.  Tesselation is done by breaking a surface into a sequence of polylines (a grid).  Pairs of polylines, possibly of differing length, describe a polygon strip.  We subdivide that strip into triangle fans and quadralateral strips, as described by the OpenGL specification.

\begin{code}
type TesselatedSurface a = [TesselatedElement a]

data TesselatedElement a = TesselatedTriangleFan [a]
                         | TesselatedQuadStrip [a]
    deriving (Read,Show)

instance (AffineTransformable a) => AffineTransformable (TesselatedElement a) where
    transform m (TesselatedTriangleFan as) = TesselatedTriangleFan $ transform m as
    transform m (TesselatedQuadStrip as) = TesselatedTriangleFan $ transform m as

instance (DeepSeq a) => DeepSeq (TesselatedElement a) where
    deepSeq (TesselatedTriangleFan as) = deepSeq as
    deepSeq (TesselatedQuadStrip as) = deepSeq as

instance Functor TesselatedElement where
    fmap f (TesselatedTriangleFan as) = TesselatedTriangleFan $ fmap f as
    fmap f (TesselatedQuadStrip as) = TesselatedQuadStrip $ fmap f as

tesselateSurface :: (ConcavityDetection a) => Surface a -> (Integer,Integer) -> TesselatedSurface a
tesselateSurface s uv = tesselateGrid $ iterateSurface uv (zipSurface (,) (fmap fst uv_identity) s)

tesselateGrid :: (ConcavityDetection a) => [[(Double,a)]] -> TesselatedSurface a
tesselateGrid = concatMap (uncurry tesselateStrip) . doubles

tesselateStrip :: (ConcavityDetection a) => [(Double,a)] -> [(Double,a)] -> TesselatedSurface a
tesselateStrip lefts rights = tesselatePieces $ tesselateSteps lefts rights

tesselateSteps :: [(Double,a)] -> [(Double,a)] -> [Either a a]
tesselateSteps lefts rights = map (either (Left . snd) (Right . snd)) $ sortBy (\x y -> compare (either fst fst x) (either fst fst y)) (map Left lefts ++ map Right rights)

tesselatePieces :: (ConcavityDetection a) => [Either a a] -> TesselatedSurface a
tesselatePieces [] = []
tesselatePieces [_] = []
tesselatePieces [_,_] = []
tesselatePieces xs = fst best : tesselatePieces (snd best)
    where rightside_triangle = tesselateAsRightSidedTriangle xs
          leftside_triangle = tesselateAsLeftSidedTriangle xs
          quadralateral = tesselateAsQuadralateral xs
          best = case (length $ fst leftside_triangle,length $ fst quadralateral,length $ fst rightside_triangle) of
                     (0,0,0) -> (undefined,[])
                     (l,q,r) | q >= r && q >= l -> first TesselatedQuadStrip quadralateral
                     (l,_,r) | l >= r -> first TesselatedTriangleFan leftside_triangle
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
tesselateAsSidedTriangle test lrs =    -- looking for a pattern that contains at least two Lefts and exactly one Right (except the test may be reversed Left for Right)
        if ok then (map stripEither $ right_vertex : (leading_lefts ++ trailing_lefts),-- the triangle strip defined by on right edge and several left edges
                    right_vertex : (last $ leading_lefts ++ trailing_lefts) : rest)      -- the right edge and the trailing left edge define the start of the next strip
              else ([],lrs)   -- pattern match failure, return the parameters as we recieved them to pattern match on something else
    where (leading_lefts,trailing) = span test lrs
          right_vertex = head trailing
          (trailing_lefts,rest) = span test $ tail trailing
          ok = (not $ null trailing) && ((not $ null leading_lefts) || (not $ null trailing_lefts))

tesselateAsQuadralateral :: (ConcavityDetection a) => [Either a a] -> ([a],[Either a a])
tesselateAsQuadralateral xs = case length goods of
                                  n | n < 4 || isConcaveQuadStrip goods -> ([],xs)
                                  n -> (\[r,l] -> (goods,Right r : Left l : trailing)) $ genericDrop (n-2) goods
    where (goods,trailing) = tesselateAsQuadralateral_ xs

tesselateAsQuadralateral_ :: [Either a a] -> ([a],[Either a a])
tesselateAsQuadralateral_ (Left l:Right r:lrs) = first ([r,l] ++) $ tesselateAsQuadralateral_ lrs
tesselateAsQuadralateral_ (Right r:Left l:lrs) = first ([r,l] ++) $ tesselateAsQuadralateral_ lrs
tesselateAsQuadralateral_ lrs = ([],lrs)
\end{code}

\subsection{Sending decomposed data to OpenGL}

\begin{code}
tesselatedElementToOpenGL :: (a -> IO ()) -> TesselatedElement a -> IO ()
tesselatedElementToOpenGL f (TesselatedQuadStrip xs) = renderPrimitive QuadStrip $ mapM_ f xs
tesselatedElementToOpenGL f (TesselatedTriangleFan xs) = renderPrimitive TriangleFan $ mapM_ f xs
\end{code}

\subsection{Concavity Detection}

When decomposing as quadralaterals, we need to detect concave polygons and decompose them as triangles instead.  Minimal definition is \texttt{toPoint3D}.

\begin{code}
class ConcavityDetection a where
    isConcave :: [a] -> Bool
    isConcave = isConcave . map toPoint3D
    toPoint3D :: a -> Point3D

instance ConcavityDetection Point3D where
    isConcave = any ((<= 0) . uncurry dotProduct) . doubles . map newell . loopedConsecutives 3
    toPoint3D = id

instance ConcavityDetection SurfaceVertex3D where
    toPoint3D = sv3d_position

isConcaveQuadStrip :: (ConcavityDetection a) => [a] -> Bool
isConcaveQuadStrip (r0:l0:r1:l1:rest) = isConcave [r0,l0,l1,r1] || isConcaveQuadStrip (r1:l1:rest)
isConcaveQuadStrip _ = False
\end{code}