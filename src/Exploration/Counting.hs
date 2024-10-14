{-# LANGUAGE ScopedTypeVariables #-}

module Exploration.Counting (
  allITFs,
  allEquivalentITFs,
  numberOfEquivalentITFs,
  averageBDDNodesITF,
) where
import           BoFun                    (BoFun)
import           Control.Monad            (replicateM)
import           Control.Monad.Free       (Free (Free, Pure))
import           Data.DecisionDiagram.BDD (numNodes)
import           Data.MultiSet            (MultiSet)
import qualified Data.MultiSet            as MultiSet
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Vector.Generic      (fromList)
import           Data.Vector.Primitive    (Vector)
import           DSLsofMath.Algebra       (Additive ((+)))
import           Prelude                  hiding ((*), (+))
import           Statistics.Sample        (meanVariance, range)
import           Subclasses.GenFun        (GenFun, liftBDD, toGenFun)
import           Subclasses.Id            ()
import           Subclasses.Iterated      (Iterated)
import           Subclasses.Threshold     (Threshold (Threshold),
                                           ThresholdFun (ThresholdFun),
                                           iteratedThresholdFunConst)
import           Test.QuickCheck          (Arbitrary (arbitrary), generate,
                                           resize)
import           Utils                    (Partition, partitions)

----------------- Iterated threshold functions ---------------------------

numberOfEquivalentITFs :: Int -> Int
numberOfEquivalentITFs = Set.size . allEquivalentITFs

-- Gives the set of equivalance classes for n-bit ITFs, where each class is represented
-- by the GenFun representation of the function.
allEquivalentITFs :: Int -> Set GenFun
allEquivalentITFs n = foldr (Set.insert . toGenFun n) Set.empty $ allITFs n

-- Gives all possible representations of n-bit ITFs, except for the ones with
-- 0-ary functions as their subfunctions, as this would lead to an infinite
-- number of representations.
-- TODO-NEW: See if we can circumvent this problem using FEAT
allITFs :: Int -> [Iterated ThresholdFun]
allITFs 0 =
  [iteratedThresholdFunConst False, iteratedThresholdFunConst True]
allITFs 1 =
  [iteratedThresholdFunConst False, iteratedThresholdFunConst True, Pure ()]
allITFs n = do
  (subFuns, nSubFuns) <- allSubFunCombinations n
  threshold' <- allThresholds nSubFuns
  return $ Free $ ThresholdFun threshold' subFuns

-- Gives all the thresholds satisfying the following properties:
-- 0 <= tn <= n + 1
-- tn + tf = n + 1
allThresholds :: Int -> [Threshold]
allThresholds n = do
  nt <- [0 .. n + 1]
  let nf = n + 1 - nt
  return $ Threshold (nt, nf)

-- Generates all possible partitions of positive integers that add up to n.
-- The member elements of these partions represent the arities of the subfunctions.
-- For each arity, we then generate all possible subFunctions.
allSubFunCombinations :: Int -> [(MultiSet (Free ThresholdFun ()), Int)]
allSubFunCombinations n = do
  partition <- partitions n
  subFuns <- mapM allITFs partition
  return (MultiSet.fromList subFuns, length subFuns)

-------------------- Counting BDD nodes --------------------------
-- ITF stands for Iterated Threshold Function

-- TODO-NEW: Dubbelkolla mot litteraturen
averageBDDNodesITF :: forall proxy f i. (Arbitrary f, BoFun f i) =>
  proxy f -> Int -> Int -> IO (Double, Double, Double)
averageBDDNodesITF _ n bits = do
  samples <- replicateM n $ generate (resize bits arbitrary) :: IO [f]
  let samples' = map (toGenFun bits) samples
  let nodeCounts = map (fromIntegral . nodeCount) samples'
  let nodeCountVector = fromList nodeCounts :: Vector Double
  let range' = range nodeCountVector
  let (mean, variance) = meanVariance nodeCountVector
  return (mean, variance, range')

nodeCount :: GenFun -> Int
nodeCount = liftBDD numNodes

