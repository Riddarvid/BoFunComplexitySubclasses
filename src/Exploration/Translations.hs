{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant evaluate" #-}
module Exploration.Translations (
  genToBasicSymmetricNaive,
  genToIteratedThresholdFun,
  funToAlg
) where
import           Algorithm.Algor             (Algor (pic, res))
import           BoFun                       (BoFun (isConst, setBit, variables))
import           Data.DecisionDiagram.BDD    (AscOrder, BDD, evaluate)
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as IM
import           Exploration.Counting        (allIteratedThresholdFunsMemo)
import           Subclasses.GenFun           (GenFun (GenFun), toGenFun)
import           Subclasses.Iterated         (Iterated)
import           Subclasses.NormalizedGenFun (NormalizedGenFun, mkNGF, ngfArity)
import           Subclasses.Symmetric        (BasicSymmetric (BasicSymmetric))
import           Subclasses.Threshold        (ThresholdFun)
import           Utils                       (permutations)

--------------- Basic symmetric ---------------------

genToBasicSymmetricNaive :: GenFun -> Maybe BasicSymmetric
genToBasicSymmetricNaive (GenFun bdd n) = do
  rv <- buildResultVector grouped bdd
  return $ BasicSymmetric rv
  where
    inputs = permutations n
    grouped = groupInputs inputs

type Input = [Bool]

groupInputs :: [Input] -> IntMap [Input]
groupInputs = foldr insertInput IM.empty

insertInput :: Input -> IntMap [Input] -> IntMap [Input]
insertInput input = IM.insertWith (++) nOnes [input]
  where
    nOnes = length $ filter id input

buildResultVector :: IntMap [Input] -> BDD AscOrder -> Maybe [Bool]
buildResultVector inputMap bdd = traverse (toResult bdd) inputsList
  where
    inputsList = map snd $ IM.toAscList inputMap

toResult :: BDD AscOrder -> [Input] -> Maybe Bool
toResult bdd inputs
  | and results = Just True
  | all not results = Just False
  | otherwise = Nothing
  where
    results = map (\input -> evaluate (\x -> input !! (x - 1)) bdd) inputs

------------ Iterated Threshold funs -----------------------

genToIteratedThresholdFun :: GenFun -> [Iterated ThresholdFun]
genToIteratedThresholdFun gf =
  filter (areEquivalent gf') $ allIteratedThresholdFunsMemo (ngfArity gf')
  where
    gf' = mkNGF gf

-- gf must be normalized
areEquivalent :: NormalizedGenFun -> Iterated ThresholdFun -> Bool
areEquivalent gf f = mkNGF (toGenFun (ngfArity gf) f) == gf

------------------- From BoFun to Algor -----------------------------

funToAlg :: (BoFun f i, Algor a) => f -> a
funToAlg = funToAlg' 0

-- If the function is not const, picks an arbitrary variable and recursively calls
-- itself for the branches.
-- Useful when converting from one function type to another.
funToAlg' :: (BoFun f i, Algor a) => Int -> f -> a
funToAlg' n f = case isConst f of
  Just val -> res val
  Nothing -> let i = head (variables f) in
    pic n (funToAlg' (n + 1) (setBit (i, False) f)) (funToAlg' (n + 1) (setBit (i, True) f))