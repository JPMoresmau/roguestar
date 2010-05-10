module Models.Tree
    (leafy_blob, tree_branch)
    where

import RSAGL.Modeling
import RSAGL.Math

leafy_blob :: ModelingM () ()
leafy_blob = model $
    do sphere (Point3D 0 0 0) 1.0
       material $ pigment $ pure forest_green

tree_branch :: ModelingM () ()
tree_branch = model $
    do closedCone (Point3D 0 0 0, 1.0) (Point3D 0 1 0, 0.5)
       material $ pigment $ pure dark_brown

