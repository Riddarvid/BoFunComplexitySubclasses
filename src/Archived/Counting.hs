-- Unused code for counting

module Archived.Counting () where

{-
module Exploration.Counting (
  allEquivalentITFs,
  numberOfEquivalentITFs
) where
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HS
import           Prelude                  hiding ((*), (+))
import           Subclasses.GenFun.GenFun (GenFun, toGenFun)
import           Subclasses.Threshold     (allNAryITFs)

----------------- Iterated threshold functions ---------------------------

numberOfEquivalentITFs :: Int -> Int
numberOfEquivalentITFs = HS.size . allEquivalentITFs

-- Gives the set of equivalance classes for n-bit ITFs, where each class is represented
-- by the GenFun representation of the function.
allEquivalentITFs :: Int -> HashSet GenFun
allEquivalentITFs n = foldr (HS.insert . toGenFun n) HS.empty $ allNAryITFs n
-}
