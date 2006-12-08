module Models.ConcordanceStarshipParts
    (concordance_material_metal,
     concordance_disk_section,
     concordance_engine_section,
     concordance_material_window,
     concordance_material_engine)
    where

import Quality
import Model
import Math3D
import System.Random

concordance_material_metal :: Texture
concordance_material_metal = SolidTexture $ rgbShine 0.15 (0.6,0.625,0.625)

concordance_material_window :: Texture
concordance_material_window = SolidTexture $ (rgbLum 0.5 (0.6,0.625,0.625)) { shine=Just 1.0 }

concordance_material_engine :: Texture
concordance_material_engine = SolidTexture $ (rgbLum 0.75 (0.0,0.1625,0.65)) { shine=Just 0.25 }

-- |
-- A passenger section of an Interstellar Concordance designed starship.
-- The parameter is a random seed by which we generate the windows.
-- (Vary the seed if you use multiple such sections on the same starship.)
--
concordance_disk_section :: Int -> Quality -> Model
concordance_disk_section seed quality =
    Union $ [
	     qualitySor quality concordance_material_metal $
	     points2d [(0,4),
		       (1,4),
		       (2,3),
		       (3,3),
		       (5,3),
		       (15,2),
		       (17.5,0.5),
		       (20,0.1),
		       (20,0),
		       (18,0),
		       (18,-0.5),
		       (17,-0.5),
		       (17,0),
		       (2,-1),
		       (0,-1)]] ++
	      (if quality >= Good
	       then
	       [dropUnionElements 25 (mkStdGen $ seed+5032) $ sor concordance_material_window 66 $ points2d [(5,3.1),(7,2.9)],
		dropUnionElements 25 (mkStdGen $ seed+7823) $ sor concordance_material_window 66 $ points2d [(9,2.7),(11,2.5)],
		dropUnionElements 25 (mkStdGen $ seed+3423) $ sor concordance_material_window 192 $ points2d [(13,2.3),(13.75,2.175)],
		dropUnionElements 25 (mkStdGen $ seed+9889) $ sor concordance_material_window 192 $ points2d [(14.25,2.225),(15,2.1)],
		dropUnionElements 50 (mkStdGen $ seed+1329) $ sor concordance_material_window 22 $ points2d [(1.35,3.75),(1.85,3.25)],
		dropUnionElements 10 (mkStdGen $ seed+9384) $ sor concordance_material_window 192 $ points2d [(16,1.5),(16.6,1.1)],
		dropUnionElements 10 (mkStdGen $ seed+1209) $ sor concordance_material_window 192 $ points2d [(16.9,0.9),(17.5,0.6)]]
	       else []
	      )
	  
concordance_engine_section :: Quality -> Model
concordance_engine_section quality =
    Union $ [
	     qualityFrame quality concordance_material_metal $ reverse $ map points3d
	     [[(-0.02,-0.02,5),
	       (-5,1,1),
	       (-5,1,-1),
	       (-3,2,-5),
	       (-2,1,-15),
	       (-1.12,0,-30)],
	      [(-0.01,-0.01,5),
	       (-5,0,1),
	       (-5,0,-1),
	       (-2,0,-5),
	       (-1,0,-15),
	       (-0.01,0,-30)],
	      [(0.01,-0.01,5),
	       (5,0,1),
	       (5,0,-1),
	       (2,0,-5),
	       (1,0,-15),
	       (0.01,0,-30)],
	      [(0.02,-0.02,5),
	       (5,1,1),
	       (5,1,-1),
	       (3,2,-5),
	       (2,1,-15),
	       (1.12,0,-30)],
	      [(0,0.01,5),  -- top edge
	       (0,1,0),
	       (0,2,0),
	       (0,3,-5),
	       (0,1,-15),
	       (0,0.01,-30)]]] ++
	      (if quality >= Good
	       then [
		     strip concordance_material_engine $
		     points3d_2 [((1,1.55,-0.5),(-1,1.55,-0.5)),
				 ((1.8,1.25,0.55),(-1.8,1.25,0.55)),
				 ((2,0.85,1.6),(-2,0.85,1.6)),
				 ((1.8,0.75,2.25),(-1.8,0.75,2.25)),
				 ((0.75,0.65,3),(-0.75,0.65,3))]]
	       else [])