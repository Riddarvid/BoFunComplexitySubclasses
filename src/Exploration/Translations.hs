{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant evaluate" #-}

-- This module isn't currently very useful, but it could be a starting point
-- for diving deeper into the topic of translating between function representations.
module Exploration.Translations (
  genToBasicSymmetricNaive,
  ngfToIteratedThresholdFun,
  boFunToGenFun,
  areEquivalent
) where
import           BDD.BDD                           (pick)
import           Complexity.BoFun                  (BoFun (isConst, setBit, variables))
import           Data.DecisionDiagram.BDD          (AscOrder, BDD, evaluate)
import           Data.IntMap                       (IntMap)
import qualified Data.IntMap                       as IM
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.List.NonEmpty                as NE
import           Subclasses.GenFun.GenFun          (GenFun (GenFun), constGF,
                                                    toGenFun)
import           Subclasses.GenFun.MinimizedGenFun (MinimizedGenFun, mkNGF,
                                                    ngfArity)
import           Subclasses.MultiComposed.Iterated (Iterated)
import           Subclasses.Symmetric              (SymmetricFun,
                                                    mkSymmetricFun)
import           Subclasses.Threshold              (NonSymmThresholdFun,
                                                    allNAryITFs)
import           Utils                             (permutations)

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
ngfToIteratedThresholdFun :: MinimizedGenFun -> [Iterated NonSymmThresholdFun]
ngfToIteratedThresholdFun gf =
  filter (areEquivalent gf) $ allNAryITFs (ngfArity gf)

areEquivalent :: MinimizedGenFun -> Iterated NonSymmThresholdFun -> Bool
areEquivalent gf f = mkNGF (toGenFun (ngfArity gf) f) == gf

------------------- From BoFun to Algor -----------------------------

-- The caller is responsible for calling with a correct arity
boFunToGenFun :: (BoFun f i) => f -> Int -> GenFun
boFunToGenFun f = GenFun bdd
  where
    (GenFun bdd _) = boFunToGenFun' 0 f

-- If the function is not const, picks an arbitrary variable and recursively calls
-- itself for the branches.
-- Useful when converting from one function type to another.
boFunToGenFun' :: (BoFun f i) => Int -> f -> GenFun
boFunToGenFun' n f = case isConst f of
  Just val -> constGF val 0
  Nothing -> let
    i = head (variables f)
    (GenFun bddF _) = boFunToGenFun' (n + 1) (setBit (i, False) f)
    (GenFun bddT _) = boFunToGenFun' (n + 1) (setBit (i, True) f)
    in GenFun (pick n bddF bddT) 0
