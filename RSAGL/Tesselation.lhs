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
import Text.Parsec.Prim
import Text.Parsec.String ()
import Data.Ord
import Control.Monad
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
tesselateStrip lefts rights = tesselate $ tesselateSteps lefts rights

data LR = L | R deriving (Eq)

otherLR :: LR -> LR
otherLR L = R
otherLR R = L

tesselateSteps :: [(Double,a)] -> [(Double,a)] -> [(LR,a)]
tesselateSteps lefts rights = map (second snd) $ sortBy (comparing $ fst . snd) $ map ((,) L) (reorder lefts) ++ map ((,) R) (reorder rights)
    where reorder :: [(Double,a)] -> [(Double,a)]
          reorder [] = []
          reorder [a] = [a]
          reorder (a:as) = a : map (\((x,_),(y,b)) -> ((x+y)/2,b)) (doubles (a:as))
\end{code}

\subsection{Tesselation Parsers}

\begin{code}
type TesselationParser a = Parsec [(LR,a)] ()

vertex :: (LR -> Bool) -> TesselationParser a a
vertex testF = liftM snd $ tokenPrim (const "") (\x _ _ -> x) (\(lr,a) -> if testF lr then Just (lr,a) else Nothing)

pushback :: [(LR,a)] -> TesselationParser a ()
pushback as =
    do setInput =<< liftM (as ++) getInput
       return ()

triangleFan :: TesselationParser a (TesselatedElement a)
triangleFan = try (triangleFanSided L) <|> try (triangleFanSided R)
    where triangleFanSided :: LR -> TesselationParser a (TesselatedElement a)
          triangleFanSided x_side =
              do let y_side = otherLR x_side
                 xs1 <- many $ vertex (== x_side)
                 y <- vertex $ (== y_side)
                 xs2 <- many $ vertex (== x_side)
                 let xs = xs1 ++ xs2
                 when (null $ drop 1 xs) $ fail "teriangleFanSided: not enough l-vertices"
                 pushback $ if null xs2 then [(x_side,last xs1),(y_side,y)] else [(y_side,y),(x_side,last xs2)]
                 return $ TesselatedTriangleFan $ case x_side of
                     L -> y:xs
                     R -> y:reverse xs

tesselate :: [(LR,a)] -> TesselatedSurface a
tesselate = either (error . ("tesselate: " ++) . show) id . runParser parser () ""
    where base_parser = triangleFan
          parser =
              do tesselated_surface <- many base_parser
                 many (vertex $ const True)
                 return tesselated_surface
\end{code}

\subsection{Sending decomposed data to OpenGL}

\begin{code}
tesselatedElementToOpenGL :: (OpenGLPrimitive a) => Bool -> TesselatedElement a -> IO ()
tesselatedElementToOpenGL colors_on (TesselatedTriangleFan xs) = renderPrimitives TriangleFan colors_on xs

unmapTesselatedElement :: TesselatedElement a -> (PrimitiveMode,[a])
unmapTesselatedElement (TesselatedTriangleFan as) = (TriangleFan,as)
\end{code}
