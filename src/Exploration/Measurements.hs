{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use list comprehension" #-}
module Exploration.Measurements (
  measureTimeGenAlg,
  measureTimePiecewiseComplexity,
  measureTimePiecewiseExplicitComplexity
) where
import           Arity                    (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun         (BoFun)
import           Complexity.GenAlg        (genAlgThinMemoPoly)
import           Complexity.Piecewise     (complexity, explicitComplexity)
import           Control.DeepSeq          (NFData, force)
import           Control.Exception        (evaluate)
import           Control.Monad            (replicateM, void)
import           Data.DecisionDiagram.BDD (numNodes)
import           Data.Function.Memoize    (Memoizable)
import           Data.Hashable            (Hashable)
import           Data.List                (sort)
import           Data.Time                (NominalDiffTime, diffUTCTime,
                                           getCurrentTime)
import           Data.Vector              (Vector, fromList)
import           Statistics.Sample        (meanVariance, range)
import           Subclasses.GenFun.GenFun (GenFun, liftBDD)
import           Test.QuickCheck          (generate)

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

measureMajs :: (a -> IO NominalDiffTime) -> (Int -> a) -> Int -> Int -> IO [(Int, NominalDiffTime)]
measureMajs timingFun majFun samples n = zip bits <$> measureFuns timingFun funs samples
  where
    bits = [1, 3 .. n]
    funs = map majFun bits

measureFuns :: (a -> IO NominalDiffTime) -> [a] -> Int -> IO [NominalDiffTime]
measureFuns timingFun funs samples = sequenceA measurements
  where
    measurements = map (medianMeasure timingFun samples) funs

medianMeasure :: (a -> IO NominalDiffTime) -> Int -> a -> IO NominalDiffTime
medianMeasure timingFun n f = do
  times <- sequenceA timingActions
  return $ median times
  where
    timingActions = replicate n (timingFun f)

----------- Generating and measuring random functions of a set arity ---------------

type NDT = NominalDiffTime

measureRandomFuns :: ArbitraryArity f =>
  (f -> IO NominalDiffTime) -> Int -> Int -> Int -> IO (NDT, NDT, NDT, NDT, NDT)
measureRandomFuns timingFun nFuns nSamples arity = do
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
    timingActions = map (medianMeasure timingFun nSamples) funs

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
