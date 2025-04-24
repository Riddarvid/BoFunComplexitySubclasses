{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- Code for interfacing with the outside, in order to avoid measurements being affected
-- by memoization.
module Exploration.Measurements.IO (
  measureRandomStdOut,
  measureSampleStdOut,
  measureThresholdStdOut,
  generateSamplesToFile
) where
import           Arity                                      (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun                           (BoFun)
import           Control.DeepSeq                            (NFData)
import           Control.Monad                              (replicateM)
import           Data.Function.Memoize                      (Memoizable)
import           Data.Hashable                              (Hashable)
import           Data.Time                                  (NominalDiffTime)
import           Exploration.Measurements                   (measureTimeGenAlg,
                                                             measureTimePiecewiseComplexity,
                                                             measureTimePiecewiseExplicitComplexity)
import           Subclasses.GenFun.CanonicalGenFun          (CanonicalGenFun)
import           Subclasses.GenFun.GenFun                   (GenFun)
import           Subclasses.GenFun.MinimizedCanonicalGenFun (MinimizedCanonicalGenFun)
import           Subclasses.GenFun.MinimizedGenFun          (MinimizedGenFun)
import           Subclasses.MultiComposed.Iterated          (Iterated)
import           Subclasses.Symmetric                       (NonSymmSymmetricFun,
                                                             SymmetricFun)
import           Subclasses.Threshold                       (NonSymmThresholdFun,
                                                             Threshold (Threshold),
                                                             ThresholdFun (ThresholdFun))
import           System.Environment                         (getArgs)
import           Test.QuickCheck                            (generate)

-- Example code for generating test cases
genSampleData :: IO ()
genSampleData = generateSamplesToFile "samples.dat" 100 [
  ("GenFun", 5),
  ("NormalizedGenFun", 5),
  ("CanonicalGenFun", 5),
  ("BothGenFun", 5),
  ("ThresholdFun", 10),
  ("ThresholdFun", 15),
  ("ThresholdFun", 150),
  ("SymmetricFun", 10),
  ("SymmetricFun", 15),
  ("SymmetricFun", 150),
  ("IterThresholdFun", 10),
  ("IterThresholdFun", 15),
  ("IterSymmetricFun", 5)
  ]

-- Generates a random function and measures the time it takes to calculate its complexity.
-- The result is output to stdout.
-- Takes as arguments the algorithm to use, the function representation type, and the arity to generate.
measureRandomStdOut :: IO ()
measureRandomStdOut = do
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
    "NormalizedGenFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO MinimizedGenFun)
    "CanonicalGenFun"  -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO CanonicalGenFun)
    "BothGenFun"       -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO MinimizedCanonicalGenFun)
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
    "NormalizedGenFun" -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO MinimizedGenFun)
    "CanonicalGenFun"  -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO CanonicalGenFun)
    "BothGenFun"       -> ImplicitMemoFun <$> (generate $ arbitraryArity arity :: IO MinimizedCanonicalGenFun)
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
    "NormalizedGenFun" -> ExplicitMemoFun <$> (generate $ arbitraryArity arity :: IO MinimizedGenFun)
    "CanonicalGenFun"  -> ExplicitMemoFun <$> (generate $ arbitraryArity arity :: IO CanonicalGenFun)
    "BothGenFun"       -> ExplicitMemoFun <$> (generate $ arbitraryArity arity :: IO MinimizedCanonicalGenFun)
    _                  -> error ("Illegel function: " ++ funStr)
  time <- measureTimePiecewiseExplicitComplexity f
  putStr $ showTime time

showTime :: NominalDiffTime -> String
showTime = init . show

--------------------------------------------

-- Measures the time it takes to calculate the given function's complexity.
-- The result is output to stdout.
-- Takes as arguments the algorithm to use, and the function to measure.
measureSampleStdOut :: IO ()
measureSampleStdOut = do
  args <- getArgs
  case args of
    [alg, funType, funStr] -> measureSampleStdOut' alg funType funStr
    _                      -> error "Wrong number of arguments"

measureSampleStdOut' :: String -> String -> String -> IO ()
measureSampleStdOut' alg funType funStr = do
  time <- case alg of
    "genAlg"             -> measureGenAlg' sampleImplicit
    "complexity"         -> measureComplexity' sampleImplicit
    "explicitComplexity" -> measureExplicitComplexity' sampleExplicit
    _                    -> error ("Unrecognized algorithm: " ++ alg)
  putStr $ showTime time
  where
    sampleImplicit = readSampleImplicitMemo funType funStr
    sampleExplicit = readSampleExplicitMemo funType funStr

----------------------------------------------

-- Simpler interface for measuring threshold functions
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

----------- Generating and parsing test samples --------------------

-- Generates a number of random functions and writes their string representations to file.
-- nSamples is the number of samples of each function type,
-- and types is a list of (function type, arity) pairs.
generateSamplesToFile :: FilePath -> Int -> [(String, Int)] -> IO ()
generateSamplesToFile fp nSamples types = do
  sampleStrings <- mapM (\(funType, arity) -> generateSampleString funType arity nSamples) types
  let outString = unlines $ map showSampleString sampleStrings
  writeFile fp outString

showSampleString :: (String, Int, String) -> String
showSampleString (funType, arity, samples) = funType ++ ", " ++ show arity ++ "\n" ++ samples ++ "\n"

data BoFunsBox = forall f i. (Show f, BoFun f i) => BoFunsBox [f]

generateSampleString :: String -> Int -> Int -> IO (String, Int, String)
generateSampleString funType arity nSamples = do
  BoFunsBox sample <- case funType of
    "GenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [GenFun])
    "NormalizedGenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [MinimizedGenFun])
    "CanonicalGenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [CanonicalGenFun])
    "BothGenFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [MinimizedCanonicalGenFun])
    "ThresholdFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [ThresholdFun])
    "SymmetricFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [SymmetricFun])
    "IterThresholdFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [Iterated NonSymmThresholdFun])
    "IterSymmetricFun" -> BoFunsBox <$> (generateSample arity nSamples :: IO [Iterated NonSymmSymmetricFun])
    _ -> error ("Unrecognized function type: " ++ funType)
  return (funType, arity, unlines $ map show sample)

generateSample :: ArbitraryArity f => Int -> Int -> IO [f]
generateSample arity nSamples = genFuns
  where
    genFun = generate $ arbitraryArity arity
    genFuns = replicateM nSamples genFun

-- Utils ---------------------------------

measureGenAlg' :: ImplicitMemoFun -> IO NominalDiffTime
measureGenAlg' (ImplicitMemoFun f) = measureTimeGenAlg f

measureComplexity' :: ImplicitMemoFun -> IO NominalDiffTime
measureComplexity' (ImplicitMemoFun f) = measureTimePiecewiseComplexity f

measureExplicitComplexity' :: ExplicitMemoFun -> IO NominalDiffTime
measureExplicitComplexity' (ExplicitMemoFun f) = measureTimePiecewiseExplicitComplexity f

readSampleImplicitMemo :: String -> String -> ImplicitMemoFun
readSampleImplicitMemo funType funStr = case funType of
  "GenFun"           -> ImplicitMemoFun (read funStr :: GenFun)
  "NormalizedGenFun" -> ImplicitMemoFun (read funStr :: MinimizedGenFun)
  "CanonicalGenFun"  -> ImplicitMemoFun (read funStr :: CanonicalGenFun)
  "BothGenFun"       -> ImplicitMemoFun (read funStr :: MinimizedCanonicalGenFun)
  "ThresholdFun"     -> ImplicitMemoFun (read funStr :: ThresholdFun)
  "SymmetricFun"     -> ImplicitMemoFun (read funStr :: SymmetricFun)
  "IterThresholdFun" -> ImplicitMemoFun (read funStr :: Iterated NonSymmThresholdFun)
  "IterSymmetricFun" -> ImplicitMemoFun (read funStr :: Iterated NonSymmSymmetricFun)
  _                  -> error ("Unrecognized fun type: " ++ funStr)

readSampleExplicitMemo :: String -> String -> ExplicitMemoFun
readSampleExplicitMemo funType funStr = case funType of
  "GenFun"           -> ExplicitMemoFun (read funStr :: GenFun)
  "NormalizedGenFun" -> ExplicitMemoFun (read funStr :: MinimizedGenFun)
  "CanonicalGenFun"  -> ExplicitMemoFun (read funStr :: CanonicalGenFun)
  "BothGenFun"       -> ExplicitMemoFun (read funStr :: MinimizedCanonicalGenFun)
  _                  -> error ("Unrecognized fun type: " ++ funStr)
