\section{Generating Noise: RSAGL.Noise}

\begin{code}
data NoiseFunctionElem = NoiseFunctionElem { nf_transformation_in :: Matrix Double,
                                             nf_transformation_out :: Double,
                                             nf_function :: Point3D -> Double }

type NoiseFunction = [NoiseFunctionElem]

instance AffineTransformable NoiseFunctionElem where
    transform m nf = nf { nf_transformation_in = (matrixInverse m) `matrixMultiply` (nf_transformation_in nf) }

noiseAt :: NoiseFunction -> Point3D -> Double
noiseAt nf p = sum $ map (flip noiseAt_ p) nf

noiseAt_ :: NoiseFunctionElem -> Point3D -> Double
noiseAt_ nf p = nf_transformation_out nf * (nf_function nf $ transform (nf_transformation_in nf) p)

-- |
-- Compose noise functions by (function,amplitude) by addition.  If the amplitudes
-- do not sum to 1, they will be scaled uniformly so that they do.
--
composeNoiseFunctions :: [(NoiseFunction,Double)] -> NoiseFunction
composeNoiseFunctions nfs =
    let total_amplitude_adjustment = 1 / (sum $ map (abs . snd) nfs)
        in concatMap (\(nf,amp) -> map (\nfe -> nfe { nf_transformation_out = nf_transformation_out nfe * total_amplitude_adjustment * amp }) nf) nfs

noiseFunction :: (Point3D -> Double) -> NoiseFunction
noiseFunction fn = [NoiseFunctionElem { nf_transformation_in = identityMatrix 4,
                                        nf_transformation_out = 1.0,
                                        nf_function = \pt -> fn pt }]

perlin_noise_function :: NoiseFunction
perlin_noise_function = noiseFunction perlinNoise

-- |
-- Given a range of lengths, synthesize a noise function from
-- layers of noise, with each layer having a frequency a given multiple
-- of the layer before it, so that the synthesized noise function contains
-- layers with many frequencies that are visible at those lengths.
--
synthesizePerlinNoise :: Double -> (Double,Double) -> NoiseFunction
synthesizePerlinNoise _ (l,r) | l > r = error "synthesizePerlinNoise: right length greater than left"
synthesizePerlinNoise m (l,r) = 
    let middle = sqrt $ l * r
        left_lengths = takeWhile (\x -> (x > l) && (x < r)) $ tail $ iterate (/ m) middle
        right_lengths = takeWhile (\x -> (x > l) && (x < r)) $ tail $ iterate (* m) middle
        lengths = left_lengths ++ [middle] ++ right_lengths
        perlin_layers = map (\x -> (scale (Vector3D x x x) perlin_noise_function,x)) lengths
        in composeNoiseFunctions perlin_layers

-- |
-- Intellectual property note:
--
-- This is based on Perlin's improved noise reference implementation.
-- I'm assuming from the manner in which it is published and the usage
-- of the term "reference implementation" that it was intended to be
-- the basis of derivative code such as this.
--
perlinNoise :: Point3D -> Double
perlinNoise (Point3D x0 y0 z0) =
   let toFloorForm (int_part,frac_part) = if frac_part < 0
                                          then (int_part - 1,1+frac_part)
                                          else (int_part,frac_part)
       (x,x') = toFloorForm $ properFraction x0
       (y,y') = toFloorForm $ properFraction y0
       (z,z') = toFloorForm $ properFraction z0
       (u,v,w) = (fade x',fade y',fade z')
       x_ = fromInteger $ x `mod` 256
       y_ = fromInteger $ y `mod` 256
       z_ = fromInteger $ z `mod` 256
       a = pRandom x_ + y_
       aa = pRandom a + z_
       ab = pRandom (a+1) + z_
       b = pRandom (x_+1) + y_
       ba = pRandom b + z_
       bb = pRandom (b+1) + z_
       in lerp w
          (lerp v (lerp u (grad (pRandom aa) x' y' z', grad (pRandom ba) (x'-1) y' z'),
                   lerp u (grad (pRandom ab) x' (y'-1) z', grad (pRandom bb) (x'-1) (y'-1) z')),
           lerp v (lerp u (grad (pRandom $ aa + 1) x' y' (z'-1), grad (pRandom $ ba + 1) (x'-1) y' (z'-1)),
                   lerp u (grad (pRandom $ ab + 1) x' (y'-1) (z'-1), grad (pRandom $ bb + 1) (x'-1) (y'-1) (z'-1))))
          / 0.75 / 2 + 0.5
                  
pRandom :: Int -> Int
pRandom n = ps ! n

ps :: Array Int Int
ps = listArray (0,511) $ map (fromInteger . (`mod` 256))
                       [646370,406894,78264,638200,196322,307448,596541,107030,4793,565585,576423,650262,571446,43108,
                        633076,111437,2644,155222,536760,617862,571480,161909,102722,312579,206645,141133,181676,284524,
                        280215,636019,266883,234557,625806,272415,484352,207495,663564,676597,612937,326007,40890,184011,
                        446726,644027,65575,547325,524216,370821,679779,512886,329446,263644,343183,308593,182572,422971,
                        563193,307537,397553,465789,106099,281992,578618,644862,655624,650433,393044,343517,621395,660924,
                        599418,211276,348504,413817,342262,6994,365076,574606,212345,556199,307227,6325,122769,23113,
                        592141,253080,125009,220837,169572,293782,441541,492375,5933,457899,92823,109194,14120,51982,
                        542438,458577,593826,397140,422154,234122,143636,408107,222278,698702,2578,59981,682795,453740,
                        173489,575340,153190,196999,691932,605064,452107,116045,513002,367422,522528,498588,231102,672824,
                        617004,445864,87729,347241,541839,389100,536575,274357,631243,688451,622729,193567,443164,389063,
                        679382,401711,446385,392253,85841,116941,378137,477578,532183,505350,166907,271925,620157,530790,
                        231601,186142,507676,657362,349234,649727,305938,348231,697574,562135,236958,78138,27017,168707,
                        89986,247657,226231,241378,52936,479391,156999,401739,226692,263422,500947,253086,570298,64452,
                        405025,378900,596296,530999,145994,322545,558501,593782,430403,11576,414070,43846,324434,78358,
                        502943,267185,590974,571378,403302,281185,388053,563007,80918,134981,1092,40952,434935,520750,
                        151444,498634,50916,215240,246514,534040,579613,648187,180807,670510,278494,595257,248865,683187,
                        345826,457619,581835,51622,597542,139805,557408,95200,621558,371979,197152,52318,513229,281452,
                        694400,641599,57994,88316,270758,612670,34072,329175,416621,80805,353671,116277,630438,290309,
                        611723,482745,22297,250872,142233,176815,300357,670684,656664,230176,442065,454494,572431,604887,
                        590406,389564,251031,687642,46790,532003,122216,20026,457476,335210,396773,237112,544900,350912,
                        666510,516275,656840,564609,586434,209022,520564,531914,266547,87172,527304,22235,352308,196285,
                        7704,693120,10172,266681,117683,443018,630909,682191,124811,366958,481565,483842,314225,106854,
                        245182,13678,118921,473039,314483,510633,166530,110489,644269,580378,279091,88039,567477,584650,
                        664533,617120,442065,238614,497131,318191,4346,440634,42414,13345,497913,329882,195683,38936,
                        682751,459807,250846,218120,688823,670973,685889,578904,525170,443495,532537,103497,30279,431158,
                        438994,165578,337400,2342,529109,201588,563954,107648,599334,219220,617589,563602,73233,183740,
                        194232,332763,268983,103095,2971,226084,695600,496718,539637,518697,564049,173337,211673,379174,
                        316485,90790,477131,387414,87217,644115,514154,194442,297690,58449,616629,244040,479291,324121,
                        115275,479931,455355,625377,132954,306518,206475,376039,505031,691073,63914,183242,664507,123147,
                        588709,406468,623129,316944,321379,22001,454851,287215,621522,324168,686373,53415,82265,389458,
                        482060,507586,239939,633044,250909,652165,285427,499157,597897,674973,341992,252169,177423,620185,
                        222801,441955,563593,136442,270903,111810,167561,135522,149272,458759,608123,393847,615496,505324,
                        339544,378541,185841,604696,537392,339813,156355,387165,500521,179032,88965,507045,322825,143658,
                        465648,388101,276598,495402,227082,69480,106879,35064,97307,639815,29054,315766,594383,613317,
                        458170,459992,407456,320487,498013,172890,290427,674191,509275,10627,235454,615307,469937,659787,
                        621266,151110,491050,286744,318109,512264,657031,75195,347477,130041,487029,214386,534258,263018,
                        656301,295905,337297,158244,729,487635,684616,253937]

fade :: (Num a) => a -> a
fade t = t ^ 3 * (t * (t * 6 - 15) + 10)

grad :: Int -> Double -> Double -> Double -> Double
grad hash x y z = 
    case hash `mod` 12 of
                       0 -> x + y
                       1 -> (-x) + y
                       2 -> x + (-y)
                       3 -> (-x) + (-y)
                       4 -> z + y
                       5 -> (-z) + y
                       6 -> z + (-y)
                       7 -> (-z) + (-y)
                       8 -> z + x
                       9 -> (-z) + x
                       10 -> z + (-x)
                       11 -> (-z) + (-x)
                       _ -> error "grad: impossible case"

--
-- End of the Perlin noise functions derived from the reference implementation.
--
\end{code}