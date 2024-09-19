{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}
module Subclasses.Comparisons (benchBoFun, complexityBench) where
import           Algorithm.GenAlgPW       (computeMin)
import           BDD                      (BDDFun)
import           BDD.BDDInstances         ()
import           BoFun                    (BoFun, eval)
import           Criterion                (Benchmark, bench, bgroup, nf)
import           Criterion.Main           (defaultMain)
import           Data.DecisionDiagram.BDD (AscOrder)
import           Data.Function.Memoize    (Memoizable)
import           Subclasses.GeneralBDD    (majBDD)
import           Subclasses.IdConst       ()
import           Subclasses.Symmetric     (maj5, symmMajBasic)
import           Test.QuickCheck          (Arbitrary (arbitrary, shrink), Gen,
                                           Property, conjoin, vector, (===))
import           Threshold                (ThresholdFun, thresholdFunReplicate,
                                           thresholdMaj)

benchBoFun :: String -> [Benchmark] -> IO ()
benchBoFun name benchmarks = defaultMain [bgroup name benchmarks]

complexityBench :: (BoFun f i, Memoizable f) => String -> f -> Benchmark
complexityBench name f = bench name $ nf computeMin f

newtype Input = Input [Bool]
  deriving (Show)

instance Arbitrary Input where
  arbitrary :: Gen Input
  arbitrary = Input <$> vector 9
  shrink :: Input -> [Input]
  shrink (Input vals) = map Input $ oneToZero vals
    where
      oneToZero []           = []
      oneToZero (True : xs)  = (False : xs) : map (True :) (oneToZero xs)
      oneToZero (False : xs) = map (False :) (oneToZero xs)

propSymmsEqual :: Input -> Property
propSymmsEqual (Input vals) = conjoin
  [
    --resSymm === resGen
    resThresh === resGen
  ]
  where
    resSymm = eval (symmMajBasic 9) (map ((), ) vals)
    resGen = eval (majBDD 9 :: BDDFun AscOrder) (zip [1 :: Int ..] vals)
    resThresh = eval (thresholdFunReplicate (thresholdMaj 5) Nothing :: ThresholdFun (Maybe Bool))
      (map (\v -> ((0, ()), v)) vals)

propMaj5Equal :: Input -> Property
propMaj5Equal (Input vals) =
  eval maj5 (map (\v -> ((0, ()), v)) vals) ===
  eval maj5Gen (zip [1 :: Int ..] vals)
  where
    maj5Gen = majBDD 5 :: BDDFun AscOrder
