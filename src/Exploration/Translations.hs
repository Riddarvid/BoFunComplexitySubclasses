{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant evaluate" #-}
module Exploration.Translations (
  genToBasicSymmetricNaive,
  ngfToIteratedThresholdFun,
  funToAlg,
  areEquivalent
) where
import           Algorithm.Algor             (Algor (pic, res))
import           BoFun                       (BoFun (isConst, setBit, variables))
import           Data.DecisionDiagram.BDD    (AscOrder, BDD, evaluate)
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as IM
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE
import           Subclasses.GenFun           (GenFun (GenFun), toGenFun)
import           Subclasses.Iterated         (IteratedSymm)
import           Subclasses.NormalizedGenFun (NormalizedGenFun, mkNGF, ngfArity)
import           Subclasses.Symmetric        (SymmetricFun, mkSymmetricFun)
import           Subclasses.Threshold        (ThresholdFun, allNAryITFs)
import           Utils                       (permutations)

--------------- Basic symmetric ---------------------

genToBasicSymmetricNaive :: GenFun -> Maybe SymmetricFun
genToBasicSymmetricNaive (GenFun bdd n) = do
  rv <- buildResultVector grouped bdd
  return $ mkSymmetricFun rv
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

buildResultVector :: IntMap [Input] -> BDD AscOrder -> Maybe (NonEmpty Bool)
buildResultVector inputMap bdd = traverse (toResult bdd) inputsList
  where
    inputsList = NE.fromList $ map snd $ IM.toAscList inputMap

toResult :: BDD AscOrder -> [Input] -> Maybe Bool
toResult bdd inputs
  | and results = Just True
  | all not results = Just False
  | otherwise = Nothing
  where
    results = map (\input -> evaluate (\x -> input !! (x - 1)) bdd) inputs

------------ Iterated Threshold funs -----------------------

-- Very inefficient, does not use the structure of the function, rather, it
-- generates all possible functions and filters out the correct ones.
ngfToIteratedThresholdFun :: NormalizedGenFun -> [IteratedSymm ThresholdFun]
ngfToIteratedThresholdFun gf =
  filter (areEquivalent gf) $ allNAryITFs (ngfArity gf)

areEquivalent :: NormalizedGenFun -> IteratedSymm ThresholdFun -> Bool
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
