{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use list comprehension" #-}
module Exploration.Comparisons (
  mainBenchMaj,
  mainBench,
  measureTimeComputeMin,
  measureTimeComputeMin',
  measureTimeGenAlg
) where
import           Algorithm.GenAlg      (genAlgThinMemo)
import           Algorithm.GenAlgPW    (computeMin, computeMin')
import           BoFun                 (BoFun)
import           Control.DeepSeq       (NFData, force)
import           Control.Exception     (evaluate)
import           Control.Monad         (forM, void)
import           Criterion             (Benchmark, bench, bgroup, nf)
import           Criterion.Main        (defaultMain)
import           Data.Function.Memoize (Memoizable)
import           Data.Hashable         (Hashable)
import           Data.Set              (Set)
import           Data.Time             (NominalDiffTime, diffUTCTime,
                                        getCurrentTime)
import           DSLsofMath.PSDS       (Poly)
import qualified Subclasses.GenFun     as Gen
import           Subclasses.Id         ()
import qualified Subclasses.Symmetric  as Symm
import qualified Subclasses.Threshold  as Thresh

-- Running a single evaluation of the complexity of a function and measuring the time it takes

data BoFunType = forall f i. (BoFun f i, Memoizable f, NFData f) => BoFunType f

measureComplexityTimes :: [BoFunType] -> IO [NominalDiffTime]
measureComplexityTimes funs = forM funs $ \(BoFunType f) -> measureTimeComputeMin f

-- Usees computeMin
measureTimeComputeMin :: (BoFun f i, Memoizable f, NFData f) => f -> IO NominalDiffTime
measureTimeComputeMin f = do
  void $ evaluate $ force f
  start <- getCurrentTime
  void $ evaluate $ force $ computeMin f
  end <- getCurrentTime
  return (diffUTCTime end start)

-- Uses computeMin'
measureTimeComputeMin' :: (BoFun f i, Hashable f, NFData f) => f -> IO NominalDiffTime
measureTimeComputeMin' f = do
  void $ evaluate $ force f
  start <- getCurrentTime
  void $ evaluate $ force $ computeMin' f
  end <- getCurrentTime
  return (diffUTCTime end start)

-- Uses genAlgMemoThin
measureTimeGenAlg :: (BoFun f i, Memoizable f, NFData f) => f -> IO NominalDiffTime
measureTimeGenAlg f = do
  void $ evaluate $ force f
  start <- getCurrentTime
  void $ evaluate $ force (genAlgThinMemo f :: Set (Poly Rational))
  end <- getCurrentTime
  return (diffUTCTime end start)

mainBenchMaj :: Int -> IO [NominalDiffTime]
mainBenchMaj = measureComplexityTimes . majFuns

majFuns :: Int -> [BoFunType]
majFuns n = [
  BoFunType $ Symm.majFun n,
  BoFunType $ Thresh.majFun n,
  BoFunType $ Gen.majFun n
  ]

--------------- Criterion code, not currently used ------------------

-- This does not seem to work, the results we get do not match our expectations.
mainBench :: Int -> IO ()
mainBench n = do
  benchBoFun name [
    --bench ("symmetric basic" ++ name) $ nf computeMin (majSymmBasic n),
    --bench ("symmetric " ++ name) $ nf computeMin (majSymm n),
    --bench ("threshold " ++ name) $ nf computeMin (maj n),
    bench ("generic " ++ name) $ nf computeMin mg
    ]
  where
    name = "maj" ++ show n
    mg = Gen.majFun n

benchBoFun :: String -> [Benchmark] -> IO ()
benchBoFun name benchmarks = defaultMain [bgroup name benchmarks]

complexityBench :: (BoFun f i, Memoizable f) => String -> f -> Benchmark
complexityBench name f = bench name $ nf computeMin f
