module Translations (
  genToBasicSymmetricNaive,
  genToIteratedThresholdFun
) where
import           Data.DecisionDiagram.BDD (AscOrder, BDD, evaluate)
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IM
import           Subclasses.Counting      (allIteratedThresholdFunsMemo)
import           Subclasses.General       (GenFun (GenFun), normalizeGenFun,
                                           toGenFun)
import           Subclasses.Iterated      (Iterated)
import           Subclasses.Symmetric     (BasicSymmetric (BasicSymmetric))
import           Subclasses.Threshold     (ThresholdFun)
import           Utils                    (permutations)

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
genToIteratedThresholdFun gf = filter (areEquivalent gf') $ allIteratedThresholdFunsMemo n
  where
    gf'@(GenFun _ n) = normalizeGenFun gf

-- gf must be normalized
areEquivalent :: GenFun -> Iterated ThresholdFun -> Bool
areEquivalent gf@(GenFun _ n) f = toGenFun n f == gf
