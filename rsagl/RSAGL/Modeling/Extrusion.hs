module RSAGL.Modeling.Extrusion
    (extrude,
     extrudeTube,
     extrudePrism)
    where

import RSAGL.Math.Curve
import RSAGL.Math.CurveExtras
import RSAGL.Math.Vector
import RSAGL.Math.Affine
import Control.Applicative
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Orthogonal
import Data.Maybe
import RSAGL.Math.Types

-- | The most general form of an extrusion.  Requires first a
-- control curve for orientation, for example the center of
-- a torus or a vector that simply never runs parallel
-- to the spine.  Second, the spine of the extrusion,
-- and third the shape to be extruded.
--
-- The +Y axis of the shape will be oriented toward
-- the control curve.
extrude :: Curve (Either Point3D Vector3D) ->
           Curve Point3D ->
           Curve (Curve Point3D) ->
           Surface Point3D
extrude upish spine loop = wrapSurface $
    transformation <$>
        (modelLookAt <$> spine <*>
                         (forward <$> Right <$> spine') <*>
                         (up <$> upish)) <*>
        loop
    where spine' = curveDerivative spine

-- | A tubular extrusion using taking a radius and a spine.
extrudeTube :: Curve RSdouble -> Curve Point3D -> Surface Point3D
extrudeTube radius spine =
    extrude upish
            spine
            (scale' <$> radius <*> pure circleXY)
    where upish = pure $ Right $ fromMaybe
                      (let [a,b] = iterateCurve 2 spine
                           in fst $ orthos $ vectorToFrom a b) $
                      newellCurve spine

-- | An extrusion whose spine is a straight line, with radii specified
-- at both ends.  In this case only the extruded shape needs to be
-- specified as a curve.
extrudePrism :: Vector3D -> (Point3D,RSdouble) -> (Point3D,RSdouble) -> Curve Point3D -> Surface Point3D
extrudePrism upish (a,ra) (b,rb) c =
    extrude (pure $ Right $ upish)
            (linearInterpolation [a,b])
            (flip scale' c <$> linearInterpolation [ra,rb])

