{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use list comprehension" #-}
module Subclasses.Comparisons (
  mainBench,
  benchBoFun,
  complexityBench,
  majEqualProp
) where
import           Algorithm.GenAlgPW    (computeMin)
import           BDD                   (bddAsc)
import           BDD.BDDInstances      ()
import           BoFun                 (BoFun, eval)
import           Criterion             (Benchmark, bench, bgroup, nf)
import           Criterion.Main        (defaultMain)
import           Data.Function.Memoize (Memoizable)
import           Subclasses.General    (majGeneral)
import           Subclasses.Id         ()
import           Subclasses.Symmetric  (majSymm, majSymmBasic)
import           Subclasses.Threshold  (ThresholdFun, majThreshold,
                                        thresholdFunReplicate, thresholdMaj)
import           Test.QuickCheck       (Arbitrary (arbitrary, shrink), Gen,
                                        Property, chooseInt, conjoin, vector,
                                        (===))

mainBench :: IO ()
mainBench = benchBoFun "maj9"
  [
    complexityBench "symmetric basic maj9" (majSymmBasic 9),
    complexityBench "symmetric maj9" (majSymm 9),
    complexityBench "threshold maj9" (thresholdFunReplicate (thresholdMaj 9) Nothing :: ThresholdFun (Maybe Bool)),
    complexityBench "generic maj9" (bddAsc $ majGeneral 9)
  ]

benchBoFun :: String -> [Benchmark] -> IO ()
benchBoFun name benchmarks = defaultMain [bgroup name benchmarks]

complexityBench :: (BoFun f i, Memoizable f) => String -> f -> Benchmark
complexityBench name f = bench name $ nf computeMin f

-- Should always have odd length
newtype MajInput = Input [Bool]
  deriving (Show)

instance Arbitrary MajInput where
  arbitrary :: Gen MajInput
  arbitrary = do
    n <- chooseInt (1, 9)
    let n' = if even n then n + 1 else n
    Input <$> vector n'
  shrink :: MajInput -> [MajInput]
  shrink (Input vals) = shorter ++ map Input (changeOneToFalse vals)
    where
      changeOneToFalse []           = []
      changeOneToFalse (True : xs)  = (False : xs) : map (True :) (changeOneToFalse xs)
      changeOneToFalse (False : xs) = map (False :) (changeOneToFalse xs)

      shorter = if length vals > 1 then [Input (drop 2 vals)] else []

majEqualProp :: MajInput -> Property
majEqualProp (Input vals) = conjoin
  [
    resSymm === resGen,
    resThresh === resGen
  ]
  where
    n = length vals
    majSymm' = majSymm n
    majGeneral' = majGeneral n
    majThreshold' = majThreshold n
    resSymm = eval majSymm'
      (map (\v -> ((0, ()), v)) vals)
    resGen = eval (bddAsc majGeneral')
      (zip [1 :: Int ..] vals)
    resThresh = eval majThreshold'
      (map (\v -> ((0, ()), v)) vals)
