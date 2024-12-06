{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Exploration.Measurements (
  measureTimeGenAlg,
  measureTimePiecewiseComplexity,
  measureTimePiecewiseExplicitComplexity,
  measureTime,
  measureSingleStdOut,
  measureSpecificStdOut,
  generateSamplesToFile,
  measureSampleStdOutArgs
) where
import           Arity                                       (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun                            (BoFun)
import           Complexity.GenAlg                           (genAlgThinMemoPoly)
import           Complexity.Piecewise                        (complexity,
                                                              explicitComplexity)
import           Control.DeepSeq                             (NFData, force)
import           Control.Exception                           (evaluate)
import           Control.Monad                               (replicateM, void)
import           Data.DecisionDiagram.BDD                    (numNodes)
import           Data.Function.Memoize                       (Memoizable)
import           Data.Hashable                               (Hashable)
import           Data.List                                   (find, sort)
import           Data.Time                                   (NominalDiffTime,
                                                              diffUTCTime,
                                                              getCurrentTime)
import           Data.Vector                                 (Vector, fromList)
import           GHC.Utils.Misc                              (split)
import           Statistics.Sample                           (meanVariance,
                                                              range)
import           Subclasses.GenFun.CanonicalGenFun           (CanonicalGenFun,
                                                              mkCGF)
import qualified Subclasses.GenFun.GenFun                    as Gen
import           Subclasses.GenFun.GenFun                    (GenFun, liftBDD)
import           Subclasses.GenFun.NormalizedCanonicalGenFun (NormalizedCanonicalGenFun,
                                                              mkNCGF)
import           Subclasses.GenFun.NormalizedGenFun          (NormalizedGenFun,
                                                              mkNGF)
import           Subclasses.Iterated.Iterated                (Iterated)
import           Subclasses.Iterated.IteratedTH              ()
import           Subclasses.LiftedTH                         ()
import qualified Subclasses.Symmetric                        as Symm
import           Subclasses.Symmetric                        (NonSymmSymmetricFun,
                                                              SymmetricFun)
import qualified Subclasses.Threshold                        as Thresh
import           Subclasses.Threshold                        (NonSymmThresholdFun,
                                                              Threshold (Threshold),
                                                              ThresholdFun (ThresholdFun))
import           System.Environment                          (getArgs)
import           Test.QuickCheck                             (generate)

------------ Measuring a single complexity calculation once ----------------------

measureTimeGenAlg :: (NFData f, Memoizable f, BoFun f i) => f -> IO NominalDiffTime
measureTimeGenAlg = measureTime genAlgThinMemoPoly

measureTimePiecewiseComplexity :: (BoFun f i, Memoizable f, NFData f) => f -> IO NominalDiffTime
measureTimePiecewiseComplexity = measureTime complexity

measureTimePiecewiseExplicitComplexity :: (BoFun f i, Hashable f, NFData f) => f -> IO NominalDiffTime
measureTimePiecewiseExplicitComplexity = measureTime explicitComplexity

measureTime :: (NFData a, NFData b) => (a -> b) -> a -> IO NominalDiffTime
measureTime f a = do
  void $ evaluate $ force a
  start <- getCurrentTime
  void $ evaluate $ force $ f a
  end <- getCurrentTime
  return (diffUTCTime end start)

----------------- Measuring a function multiple times and taking the median -----------------

{-
measureMajs :: (a -> IO NominalDiffTime) -> (Int -> a) -> Int -> Int -> IO [(Int, NominalDiffTime)]
measureMajs timingFun majFun samples n = zip bits <$> measureFuns timingFun funs samples
  where
    bits = [1, 3 .. n]
    funs = map majFun bits

measureFuns :: (a -> IO NominalDiffTime) -> [a] -> Int -> IO [NominalDiffTime]
measureFuns timingFun funs samples = sequenceA measurements
  where
    measurements = map (medianMeasure timingFun samples) funs-}

medianMeasure :: (a -> IO NominalDiffTime) -> Int -> a -> IO NominalDiffTime
medianMeasure timingFun n f = do
  times <- sequenceA timingActions
  return $ median times
  where
    timingActions = replicate n (timingFun f)

----------- Generating and measuring random functions of a set arity ---------------

-- Not used since memoization interferred with the measurements. See external python script
-- instead.
{-
type NDT = NominalDiffTime

measureRandom :: ArbitraryArity f =>
  (f -> IO NominalDiffTime) -> Int -> Int -> IO ()
measureRandom timingFun nFuns arity = do
  funs <- genFuns
  times <- mapM timingFun funs
  let str = unlines $ map (take 6 . show) times
  writeFile "data" str
  where
    genFun = generate $ arbitraryArity arity
    genFuns = replicateM nFuns genFun

measureRandomFiveValue :: ArbitraryArity f =>
  (f -> IO NominalDiffTime) -> Int -> Int -> Int -> IO (NDT, NDT, NDT, NDT, NDT)
measureRandomFiveValue timingFun nFuns nSamples arity = do
  funs <- genFuns
  measureFiveValues timingFun nSamples funs
  where
    genFun = generate $ arbitraryArity arity
    genFuns = replicateM nFuns genFun

measureFiveValues :: (a -> IO NominalDiffTime) -> Int -> [a] ->
  IO (NDT, NDT, NDT, NDT, NDT)
measureFiveValues timingFun nSamples funs = do
  sample <- sequenceA timingActions
  return $ fiveValueSummary sample
  where
    timingActions = map printTimeId funs
    printTimeId f = do
      time <- medianMeasure timingFun nSamples f
      print time
      return time-}

----------- Measuring average number of nodes in a BDD --------------

averageBDDNodesITF :: Int -> Int -> IO (Double, Double, Double)
averageBDDNodesITF arity nSamples = do
  samples <- replicateM nSamples $ generate (arbitraryArity arity) :: IO [GenFun]
  let nodeCounts = map (fromIntegral . nodeCount) samples
  let nodeCountVector = fromList nodeCounts :: Vector Double
  let range' = range nodeCountVector
  let (mean, variance) = meanVariance nodeCountVector
  return (mean, variance, range')

nodeCount :: GenFun -> Int
nodeCount = liftBDD numNodes

------------- Utils ------------------------

median :: (Ord a, Fractional a) => [a] -> a
median times
  | odd lt = times' !! mid
  | otherwise = (times' !! mid' + times' !! mid) / 2
  where
    times' = sort times
    lt = length times'
    mid = lt `div` 2
    mid' = mid - 1

splitSample :: [a] -> ([a], [a])
splitSample sample = (low, high')
  where
    ls = length sample
    mid = ls `div` 2
    (low, high) = splitAt mid sample
    high' = if odd ls then tail high else high

fiveValueSummary :: (Ord a, Fractional a) => [a] -> (a, a, a, a, a)
fiveValueSummary sample = (min', median low, median', median high, max')
  where
    sample' = sort sample
    min' = head sample'
    max' = last sample'
    median' = median sample'
    (low, high) = splitSample sample'

------------- Code for interfacing with the outside -----------

-- Args: alg, function type, arity
measureSingleStdOut :: IO ()
measureSingleStdOut = do
  args <- getArgs
  let funStr = args !! 1
  let arity = read (args !! 2)
  case head args of
    "genAlg"             -> benchmarkGenAlg funStr arity
    "complexity"         -> benchmarkComplexity funStr arity
    "explicitComplexity" -> benchmarkExplicitComplexity funStr arity
    alg                  -> error ("Invalid alg: " ++ show alg)

data ImplicitMemoFun = forall f i. (NFData f, Memoizable f, BoFun f i) => ImplicitMemoFun f

data ExplicitMemoFun = forall f i. (NFData f, Hashable f, BoFun f i) => ExplicitMemoFun f

benchmarkGenAlg :: String -> Int -> IO ()
benchmarkGenAlg funStr arity = do
  ImplicitMemoFun f <- case funStr of
    "GenFun"           -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO GenFun)
    "NormalizedGenFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO NormalizedGenFun)
    "CanonicalGenFun"  -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO CanonicalGenFun)
    "BothGenFun"       -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO NormalizedCanonicalGenFun)
    "ThresholdFun"     -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO ThresholdFun)
    "SymmetricFun"     -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO SymmetricFun)
    "IterThresholdFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO (Iterated NonSymmThresholdFun))
    "IterSymmetricFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO (Iterated NonSymmSymmetricFun))
    _                  -> error ("Illegel function: " ++ funStr)
  time <- measureTimeGenAlg f
  putStr $ showTime time

benchmarkComplexity :: String -> Int -> IO ()
benchmarkComplexity funStr arity = do
  ImplicitMemoFun f <- case funStr of
    "GenFun"           -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO GenFun)
    "NormalizedGenFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO NormalizedGenFun)
    "CanonicalGenFun"  -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO CanonicalGenFun)
    "BothGenFun"       -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO NormalizedCanonicalGenFun)
    "ThresholdFun"     -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO ThresholdFun)
    "SymmetricFun"     -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO SymmetricFun)
    "IterThresholdFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO (Iterated NonSymmThresholdFun))
    "IterSymmetricFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO (Iterated NonSymmSymmetricFun))
    _                  -> error ("Illegel function: " ++ funStr)
  time <- measureTimePiecewiseComplexity f
  putStr $ showTime time

benchmarkExplicitComplexity :: String -> Int -> IO ()
benchmarkExplicitComplexity funStr arity = do
  ExplicitMemoFun f <- case funStr of
    "GenFun"           -> ExplicitMemoFun <$> (generate $ arbitraryArity arity :: IO GenFun)
    "NormalizedGenFun" -> ExplicitMemoFun <$> (generate $ arbitraryArity arity :: IO NormalizedGenFun)
    "CanonicalGenFun"  -> ExplicitMemoFun <$> (generate $ arbitraryArity arity :: IO CanonicalGenFun)
    "BothGenFun"       -> ExplicitMemoFun <$> (generate $ arbitraryArity arity :: IO NormalizedCanonicalGenFun)
    _                  -> error ("Illegel function: " ++ funStr)
  time <- measureTimePiecewiseExplicitComplexity f
  putStr $ showTime time

showTime :: NominalDiffTime -> String
showTime = init . show

measureSpecificStdOut :: IO ()
measureSpecificStdOut = do
  args <- getArgs
  let funTypStr = args !! 1
  let funStr = args !! 2
  case head args of
    "genAlg"             -> benchmarkSpecificGenAlg funTypStr funStr
    "complexity"         -> benchmarkSpecificComplexity funTypStr funStr
    "explicitComplexity" -> benchmarkSpecificExplicitComplexity funTypStr funStr
    alg                  -> error ("Invalid alg: " ++ show alg)

benchmarkSpecificGenAlg :: String -> String -> IO ()
benchmarkSpecificGenAlg funTypeStr funStr = do
  let f = getImplicitMemoFun funTypeStr funStr
  time <- measureGenAlg' f
  putStr $ showTime time

benchmarkSpecificComplexity :: String -> String -> IO ()
benchmarkSpecificComplexity funTypeStr funStr = do
  let f = getImplicitMemoFun funTypeStr funStr
  time <- measureComplexity' f
  putStr $ showTime time

benchmarkSpecificExplicitComplexity :: String -> String -> IO ()
benchmarkSpecificExplicitComplexity funTypeStr funStr = do
  let f = getExplicitMemoFun funTypeStr funStr
  time <- measureExplicitComplexity' f
  putStr $ showTime time

getImplicitMemoFun :: String -> String -> ImplicitMemoFun
getImplicitMemoFun funTypeStr funStr = case (funTypeStr, head fun) of
  ("GenFun", "maj")           -> ImplicitMemoFun $ Gen.majFun majN
  ("NormalizedGenFun", "maj") -> ImplicitMemoFun $ mkNGF $ Gen.majFun majN
  ("CanonicalGenFun", "maj")  -> ImplicitMemoFun $ mkCGF $ Gen.majFun majN
  ("BothGenFun", "maj")       -> ImplicitMemoFun $ mkNCGF $ Gen.majFun majN
  ("ThresholdFun", "maj")     -> ImplicitMemoFun $ Thresh.majFun majN
  ("SymmetricFun", "maj")     -> ImplicitMemoFun $ Symm.majFun majN
  ("IterThresholdFun", "maj") -> ImplicitMemoFun $ Thresh.iteratedMajFun majN 1
  ("IterSymmetricFun", "maj") -> ImplicitMemoFun $ Symm.iteratedMajFun majN 1
  ("GenFun", "iterMaj")           -> ImplicitMemoFun $ Gen.iteratedMajFun iterMajBits iterMajLevels
  ("NormalizedGenFun", "iterMaj") -> ImplicitMemoFun $ mkNGF $ Gen.iteratedMajFun iterMajBits iterMajLevels
  ("CanonicalGenFun", "iterMaj")  -> ImplicitMemoFun $ mkCGF $ Gen.iteratedMajFun iterMajBits iterMajLevels
  ("BothGenFun", "iterMaj")       -> ImplicitMemoFun $ mkNCGF $ Gen.iteratedMajFun iterMajBits iterMajLevels
  ("ThresholdFun", "iterMaj")     -> ImplicitMemoFun $ Thresh.iteratedMajFun iterMajBits iterMajLevels
  ("SymmetricFun", "iterMaj")     -> ImplicitMemoFun $ Symm.iteratedMajFun iterMajBits iterMajLevels
  ("IterThresholdFun", "iterMaj") -> ImplicitMemoFun $ Thresh.iteratedMajFun iterMajBits iterMajLevels
  ("IterSymmetricFun", "iterMaj") -> ImplicitMemoFun $ Symm.iteratedMajFun iterMajBits iterMajLevels
  _ -> error "Invalid function"
  where
    fun = parseFun funStr
    majN = case fun of
      [_, s] -> read s
      _      -> error "Wrong number of function args"
    (iterMajBits, iterMajLevels) = case fun of
      [_, s1, s2] -> (read s1, read s2)
      _           -> error "Wrong number of function args"

getExplicitMemoFun :: String -> String -> ExplicitMemoFun
getExplicitMemoFun funTypeStr funStr = case (funTypeStr, head fun) of
  ("GenFun", "maj")           -> ExplicitMemoFun $ Gen.majFun majN
  ("NormalizedGenFun", "maj") -> ExplicitMemoFun $ mkNGF $ Gen.majFun majN
  ("CanonicalGenFun", "maj")  -> ExplicitMemoFun $ mkCGF $ Gen.majFun majN
  ("BothGenFun", "maj")       -> ExplicitMemoFun $ mkNCGF $ Gen.majFun majN
  ("GenFun", "iterMaj")           -> ExplicitMemoFun $ Gen.iteratedMajFun iterMajBits iterMajLevels
  ("NormalizedGenFun", "iterMaj") -> ExplicitMemoFun $ mkNGF $ Gen.iteratedMajFun iterMajBits iterMajLevels
  ("CanonicalGenFun", "iterMaj")  -> ExplicitMemoFun $ mkCGF $ Gen.iteratedMajFun iterMajBits iterMajLevels
  ("BothGenFun", "iterMaj")       -> ExplicitMemoFun $ mkNCGF $ Gen.iteratedMajFun iterMajBits iterMajLevels
  _ -> error ("Invalid function: " ++ show (funTypeStr, funStr))
  where
    fun = parseFun funStr
    majN = read (fun !! 1)
    iterMajBits = read (fun !! 1)
    iterMajLevels = read (fun !! 2)

measureGenAlg' :: ImplicitMemoFun -> IO NominalDiffTime
measureGenAlg' (ImplicitMemoFun f) = measureTimeGenAlg f

measureComplexity' :: ImplicitMemoFun -> IO NominalDiffTime
measureComplexity' (ImplicitMemoFun f) = measureTimePiecewiseComplexity f

measureExplicitComplexity' :: ExplicitMemoFun -> IO NominalDiffTime
measureExplicitComplexity' (ExplicitMemoFun f) = measureTimePiecewiseExplicitComplexity f

parseFun :: String -> [String]
parseFun = split '_'

----------- Generating and parsing test samples --------------------

generateSamplesToFile :: FilePath -> Int -> [(String, Int)] -> IO ()
generateSamplesToFile fp nSamples types = do
  sampleStrings <- mapM (\(funType, arity) -> generateSampleString funType arity nSamples) types
  let outString = unlines $ map showSampleString sampleStrings
  putStr outString
  --writeFile fp outString

showSampleString :: (String, Int, String) -> String
showSampleString (funType, arity, samples) = funType ++ ", " ++ show arity ++ "\n" ++ samples ++ "\n"

generateSampleString :: String -> Int -> Int -> IO (String, Int, String)
generateSampleString funType arity nSamples = do
  BoFunsBox sample <- case funType of
    "GenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [GenFun])
    "NormalizedGenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [NormalizedGenFun])
    "CanonicalGenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [CanonicalGenFun])
    "BothGenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [NormalizedCanonicalGenFun])
    "ThresholdFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [ThresholdFun])
    "SymmetricFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [SymmetricFun])
    "IterThresholdFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [Iterated NonSymmThresholdFun])
    "IterSymmetricFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [Iterated NonSymmSymmetricFun])
    _ -> error ("Unrecognized function type: " ++ funType)
  return (funType, arity, unlines $ map show sample)

data BoFunsBox = forall f i. (Show f, BoFun f i) => BoFunsBox [f]

generateSample :: ArbitraryArity f => Int -> Int -> IO [f]
generateSample arity nSamples = genFuns
  where
    genFun = generate $ arbitraryArity arity
    genFuns = replicateM nSamples genFun

---------------------------------------------------

measureSampleStdOutArgs :: IO ()
measureSampleStdOutArgs = do
  args <- getArgs
  case args of
    [alg, funType, funStr] -> measureSampleStdOut alg funType funStr
    _                      -> error "Wrong number of arguments"

measureSampleStdOut :: String -> String -> String -> IO ()
measureSampleStdOut alg funType funStr = do
  time <- case alg of
    "genAlg"             -> measureGenAlg' sampleImplicit
    "complexity"         -> measureComplexity' sampleImplicit
    "explicitComplexity" -> measureExplicitComplexity' sampleExplicit
    _                    -> error ("Unrecognized algorithm: " ++ alg)
  putStr $ showTime time
  where
    sampleImplicit = readSampleImplicitMemo funType funStr
    sampleExplicit = readSampleExplicitMemo funType funStr

readSampleImplicitMemo :: String -> String -> ImplicitMemoFun
readSampleImplicitMemo funType funStr = case funType of
  "GenFun"           -> ImplicitMemoFun (read funStr :: GenFun)
  "NormalizedGenFun" -> ImplicitMemoFun (read funStr :: NormalizedGenFun)
  "CanonicalGenFun"  -> ImplicitMemoFun (read funStr :: CanonicalGenFun)
  "BothGenFun"       -> ImplicitMemoFun (read funStr :: NormalizedCanonicalGenFun)
  "ThresholdFun"     -> ImplicitMemoFun (read funStr :: ThresholdFun)
  "SymmetricFun"     -> ImplicitMemoFun (read funStr :: SymmetricFun)
  "IterThresholdFun" -> ImplicitMemoFun (read funStr :: Iterated NonSymmThresholdFun)
  "IterSymmetricFun" -> ImplicitMemoFun (read funStr :: Iterated NonSymmSymmetricFun)
  _                  -> error ("Unrecognized fun type: " ++ funStr)

readSampleExplicitMemo :: String -> String -> ExplicitMemoFun
readSampleExplicitMemo funType funStr = case funType of
  "GenFun"           -> ExplicitMemoFun (read funStr :: GenFun)
  "NormalizedGenFun" -> ExplicitMemoFun (read funStr :: NormalizedGenFun)
  "CanonicalGenFun"  -> ExplicitMemoFun (read funStr :: CanonicalGenFun)
  "BothGenFun"       -> ExplicitMemoFun (read funStr :: NormalizedCanonicalGenFun)
  _                  -> error ("Unrecognized fun type: " ++ funStr)

measureThresholdStdOut :: IO ()
measureThresholdStdOut = do
  args <- getArgs
  case args of
    [nt, nf] -> measureThresholdStdOut' (ThresholdFun (Threshold (read nt, read nf)))
    _ -> error "Wrong number of arguments"

measureThresholdStdOut' :: ThresholdFun -> IO ()
measureThresholdStdOut' tf = do
  time <- measureTimePiecewiseComplexity tf
  putStr $ showTime time
