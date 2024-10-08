{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use list comprehension" #-}
module Subclasses.Comparisons (
  propMajEqual,
  propSameComplexity,
  mainBenchMaj,
  mainBench,
  measureComplexityTime
) where
import           Algorithm.GenAlgPW    (computeMin)
import           BoFun                 (BoFun)
import           Control.DeepSeq       (force)
import           Control.Exception     (evaluate)
import           Control.Monad         (forM, void)
import           Criterion             (Benchmark, bench, bgroup, nf)
import           Criterion.Main        (defaultMain)
import           Data.Function.Memoize (Memoizable)
import           Data.Time             (NominalDiffTime, diffUTCTime,
                                        getCurrentTime)
import qualified Subclasses.General    as Gen
import           Subclasses.General    (eval, toGenFun)
import           Subclasses.Id         ()
import qualified Subclasses.Symmetric  as Symm
import qualified Subclasses.Threshold  as Thresh
import           Test.QuickCheck       (Arbitrary (arbitrary, shrink), Gen,
                                        Property, chooseInt, conjoin, sized,
                                        vector, (===))

data BoFunType = forall f i. (BoFun f i, Memoizable f) => BoFunType f

mainBenchMaj :: Int -> IO [NominalDiffTime]
mainBenchMaj = measureComplexityTimes . majFuns

majFuns :: Int -> [BoFunType]
majFuns n = [
  BoFunType $ Symm.majFunBasic n,
  BoFunType $ Symm.majFun n,
  BoFunType $ Thresh.majFun n,
  BoFunType $ Gen.majFun n
  ]

measureComplexityTimes :: [BoFunType] -> IO [NominalDiffTime]
measureComplexityTimes funs = forM funs $ \(BoFunType f) -> measureComplexityTime f

measureComplexityTime :: (BoFun f i, Memoizable f) => f -> IO NominalDiffTime
measureComplexityTime f = do
  start <- getCurrentTime
  void $ evaluate $ force $ computeMin f
  end <- getCurrentTime
  return (diffUTCTime end start)

-- Should always have odd length
newtype MajInput = Input [Bool]
  deriving (Show)

instance Arbitrary MajInput where
  arbitrary :: Gen MajInput
  arbitrary = sized $ \n -> do
    n' <- chooseInt (1, n)
    let n'' = if even n' then n' + 1 else n'
    Input <$> vector n''
  shrink :: MajInput -> [MajInput]
  shrink (Input [False]) = []
  shrink (Input [True]) = [Input [False]]
  shrink (Input vals) = [Input [False], Input [True]] ++ shorter ++ map Input (changeOneToFalse vals)
    where
      changeOneToFalse []           = []
      changeOneToFalse (True : xs)  = (False : xs) : map (True :) (changeOneToFalse xs)
      changeOneToFalse (False : xs) = map (False :) (changeOneToFalse xs)

      shorter = if length vals > 1 then [Input (drop 2 vals)] else []

propMajEqual :: MajInput -> Property
propMajEqual (Input vals) = conjoin
  [
    resSymm === resGen,
    resThresh === resGen
  ]
  where
    n = length vals
    majGeneral = Gen.majFun n
    majSymm = toGenFun n $ Symm.majFun n
    majThreshold = toGenFun n $ Thresh.majFun n
    resSymm = eval majSymm vals
    resGen = eval majGeneral vals
    resThresh = eval majThreshold vals

propSameComplexity :: Property
propSameComplexity = conjoin
  [
    symm === gen,
    thresh === gen
  ]
  where
    gen = computeMin $ Gen.iteratedMajFun 3 2
    symm = computeMin $ Symm.iteratedMajFun 3 2
    thresh = computeMin $ Thresh.iteratedMajFun 3 2

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
