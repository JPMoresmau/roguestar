module Models.Tree
    (leafy_tree)
    where

import RSAGL.Modeling
import RSAGL.Math
import Control.Monad.Random
import Quality
import Control.Monad.State

leafy_tree :: Quality -> Modeling ()
leafy_tree Bad = evalRandT (leafyTreeBranch origin_point_3d (Vector3D 0 0.5 0) 0.1 2) (mkStdGen 45)
leafy_tree Poor = evalRandT (leafyTreeBranch origin_point_3d (Vector3D 0 0.5 0) 0.1 3) (mkStdGen 45)
leafy_tree Good = evalRandT (leafyTreeBranch origin_point_3d (Vector3D 0 0.5 0) 0.1 4) (mkStdGen 45)
leafy_tree Super = evalRandT (leafyTreeBranch origin_point_3d (Vector3D 0 0.5 0) 0.1 5) (mkStdGen 45)

leafyTreeBranch :: Point3D -> Vector3D -> Double -> Int -> RandT StdGen (ModelingM ()) ()
leafyTreeBranch point vector thickness recursion | recursion <= 0 = 
     do b <- getRandom
        when b $ lift $ model $
            do sphere point (vectorLength vector + thickness)
	       return vector
	       return thickness
	       return point
               material $ pigment $ pure forest_green
leafyTreeBranch point vector thickness recursion =
    do lift $ model $
           do closedCone (point,thickness) (translate vector point,thickness/2)
	      material $ pigment $ pure dark_brown
       us <- liftM (take recursion) $ getRandomRs (0.0,1.0)
       mapM leafyTreeBranchFrom us
       leafyTreeBranchFrom 1.0
  where leafyTreeBranchFrom :: Double -> RandT StdGen (ModelingM ()) ()
        leafyTreeBranchFrom u =
	    do let new_vector_constraint = vectorLength vector / 1.5
	       (x:y:z:_) <- getRandomRs (-new_vector_constraint,new_vector_constraint)
	       t <- getRandomR (thickness/3,thickness/2)
               leafyTreeBranch (lerp u (point,translate vector point)) (vectorScaleTo new_vector_constraint $ vector `add` (Vector3D x y z)) t (recursion - 1)
