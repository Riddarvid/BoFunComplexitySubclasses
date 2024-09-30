{-# LANGUAGE InstanceSigs              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Subclasses.Comparisons (
  propMajEqual,
  propSameComplexity,
  measureComplexityTime,
  mainBenchMaj,
  mainBench
) where
import           Algorithm.GenAlgPW    (computeMin)
import           BDD                   (bddAsc)
import           BDD.BDDInstances      ()
import           BoFun                 (BoFun, eval)
import           Control.DeepSeq       (force)
import           Control.Exception     (evaluate)
import           Control.Monad         (forM)
import           Criterion             (Benchmark, bench, bgroup, nf)
import           Criterion.Main        (defaultMain)
import           Data.Function.Memoize (Memoizable)
import           Data.Time             (NominalDiffTime, diffUTCTime,
                                        getCurrentTime)
import qualified Subclasses.General    as Gen
import           Subclasses.Id         ()
import qualified Subclasses.Symmetric  as Symm
import qualified Subclasses.Threshold  as Thresh
import           Test.QuickCheck       (Arbitrary (arbitrary, shrink), Gen,
                                        Property, chooseInt, conjoin, resize,
                                        sized, vector, (===))

data BoFunType = forall f i. (BoFun f i, Memoizable f) => BoFunType f

mainBenchMaj :: Int -> IO [NominalDiffTime]
mainBenchMaj = measureComplexityTime . majFuns

majFuns :: Int -> [BoFunType]
majFuns n = [
  BoFunType $ Symm.majFunBasic n,
  BoFunType $ Symm.majFun n,
  BoFunType $ Thresh.majFun n,
  BoFunType $ Gen.majFun n
  ]

measureComplexityTime :: [BoFunType] -> IO [NominalDiffTime]
measureComplexityTime funs = forM funs $ \(BoFunType f) -> do
  start <- getCurrentTime
  _ <- evaluate $ force $ computeMin f
  end <- getCurrentTime
  return (diffUTCTime end start)

-- Should always have odd length
newtype MajInput = Input [Bool]
  deriving (Show)

instance Arbitrary MajInput where
  arbitrary :: Gen MajInput
  arbitrary = resize 10 $ sized $ \n -> do
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
    majSymm = Symm.majFun n
    majGeneral = Gen.majFun n
    majThreshold = Thresh.majFun n
    resSymm = eval majSymm
      (map (\v -> ((0, ()), v)) vals)
    resGen = eval (bddAsc majGeneral)
      (map (\v -> (0, v)) vals)
    resThresh = eval majThreshold
      (map (\v -> ((0, ()), v)) vals)

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
