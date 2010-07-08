{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies, FlexibleContexts #-}

module AnimationTerrain
    (terrainThreadLauncher)
    where

import Data.List
import RSAGL.Math.Types
import RSAGL.FRP
import RSAGL.Math
import RSAGL.Animation.InverseKinematics
import Animation
import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Data.Maybe
import Models.LibraryData
import ProtocolTypes
import Scene
import AnimationExtras

type TerrainThreadSwitch m = RSwitch Enabled (Maybe ProtocolTypes.TerrainTile) () () m

-- | Thread that launches rendering threads for the terrain tiles.
terrainThreadLauncher :: (FRPModel m) => FRP e (TerrainThreadSwitch m) () ()
terrainThreadLauncher = spawnThreads <<<
                        arr (map (\x -> (Just x,terrainTile x))) <<<
                        newListElements <<< terrainElements

terrainTile :: (FRPModel m) => ProtocolTypes.TerrainTile ->
                               FRP e (TerrainThreadSwitch m) () ()
terrainTile (tid@(ProtocolTypes.TerrainTile terrain_type (x,y))) = proc () ->
    do terrain_elements <- terrainElements -< ()
       let still_here = isJust $ find (== tid) terrain_elements
       let goal_size = if still_here then 1.25 else -0.25
       actual_size <- arr (max 0 . min 1) <<<
                      approachFrom 0.5 (perSecond 1.0) 0 -< goal_size
       killThreadIf -< actual_size <= 0.0 && not still_here
       transformA (libraryA >>> terrainDecoration tid) -<
           (Affine $ translate
                         (Vector3D (fromInteger x) 0 (negate $ fromInteger y)) .
                     scale' actual_size,
               (scene_layer_local,Models.LibraryData.TerrainTile terrain_type))
       returnA -< ()

terrainElements :: (FRPModel m, StateOf m ~ AnimationState) => FRP e m () [ProtocolTypes.TerrainTile]
terrainElements = arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA <<< arr (const ("visible-terrain","0"))

terrainDecoration :: (FRPModel m, StateOf m ~ AnimationState) =>
                     ProtocolTypes.TerrainTile ->
                     FRP e m () ()
terrainDecoration (ProtocolTypes.TerrainTile "forest" (x,y)) =
    fst $ runRand (leafyTree 2 True)
                  (mkStdGen $ fromInteger $ x + 1000*y)
terrainDecoration (ProtocolTypes.TerrainTile "deepforest" (x,y)) =
    fst $ runRand (leafyTree 3 True)
                  (mkStdGen $ fromInteger $ 2*x + 1001*y + 7)
terrainDecoration _ = proc () -> returnA -< ()

leafyTree :: (FRPModel m, StateOf m ~ AnimationState) =>
             Int -> Bool -> Rand StdGen (FRP e m () ())
leafyTree recursion has_leaves =
    do dead_tree <- liftM (== 1) $ getRandomR (1,4 :: Integer)
       (x:y:_) <- getRandomRs (-0.4,0.4)
       thickness <- getRandomR (0.02*realToFrac recursion,
                                0.04*realToFrac recursion)
       push_up <- getRandomR (1.5/realToFrac recursion,
                              3.0/realToFrac recursion)
       leafyTreeBranch (Point3D x 0 y)
                       (Vector3D 0 push_up 0)
                       thickness
                       recursion
                       (has_leaves && not dead_tree)

leafyTreeBranch :: (FRPModel m, StateOf m ~ AnimationState) =>
                   Point3D ->
                   Vector3D ->
                   RSdouble ->
                   Int ->
                   Bool ->
                   Rand StdGen (FRP e m () ())
leafyTreeBranch point vector thickness recursion has_leaves | recursion <= 0 =
     do let leaves = translateToFrom point (Point3D 0 0 0) $
                scale' (vectorLength vector + thickness) $
                    proc () -> libraryA -< (scene_layer_local,LeafyBlob)
        return $ if has_leaves then leaves else proc () -> returnA -< ()
leafyTreeBranch point vector thickness recursion has_leaves =
    do b <- getRandom
       let branch_inset = min 0.25 $ thickness / vectorLength vector
       takes <- getRandomR (1,recursion)
       us <- liftM (take takes) $ getRandomRs (2*branch_inset,1.0-branch_inset)
       other_branches <- mapM (leafyTreeBranchFrom $ b && has_leaves) us
       continue_trunk <- leafyTreeBranchFrom has_leaves $ 1.0 - branch_inset
       let this_branch = translateToFrom point (Point3D 0 0 0) $
               rotateToFrom vector (Vector3D 0 1 0) $
                   scale (Vector3D thickness (vectorLength vector) thickness) $
                       proc () -> libraryA -< (scene_layer_local,TreeBranch)
       return $ this_branch >>> continue_trunk >>> foldr1 (>>>) other_branches
  where leafyTreeBranchFrom :: (FRPModel m, StateOf m ~ AnimationState) =>
                               Bool -> RSdouble -> Rand StdGen (FRP e m () ())
        leafyTreeBranchFrom pass_leaves u =
            do let new_vector_constraint = vectorLength vector / 1.5
               (x:y:z:_) <- getRandomRs (-new_vector_constraint,
                                          new_vector_constraint)
               t <- getRandomR (thickness/3,thickness/2)
               leafyTreeBranch
                   (lerp u (point,translate vector point))
                   (vectorScaleTo new_vector_constraint $
                       vector `add` (Vector3D x y z))
                   t
                   (recursion - 1)
                   pass_leaves

