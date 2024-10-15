{-# LANGUAGE ScopedTypeVariables #-}

module Exploration.Counting (
  allEquivalentITFs,
  numberOfEquivalentITFs,
  averageBDDNodesITF,
) where
import           BoFun                    (BoFun)
import           Control.Monad            (replicateM)
import           Data.DecisionDiagram.BDD (numNodes)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Vector.Generic      (fromList)
import           Data.Vector.Primitive    (Vector)
import           Prelude                  hiding ((*), (+))
import           Statistics.Sample        (meanVariance, range)
import           Subclasses.GenFun        (GenFun, liftBDD, toGenFun)
import           Subclasses.Id            ()
import           Subclasses.Threshold     (allNAryITFs)
import           Test.QuickCheck          (Arbitrary (arbitrary), generate,
                                           resize)

----------------- Iterated threshold functions ---------------------------

numberOfEquivalentITFs :: Int -> Int
numberOfEquivalentITFs = Set.size . allEquivalentITFs

-- Gives the set of equivalance classes for n-bit ITFs, where each class is represented
-- by the GenFun representation of the function.
allEquivalentITFs :: Int -> Set GenFun
allEquivalentITFs n = foldr (Set.insert . toGenFun n) Set.empty $ allNAryITFs n

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

