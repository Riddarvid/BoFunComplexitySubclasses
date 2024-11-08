{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant evaluate" #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Exploration.Translations (
  genToBasicSymmetricNaive,
  ngfToIteratedThresholdFun,
  areEquivalent,
  boFunToGenFuns,
  allRepresentableFuns
) where
import           Arity                       (AllArity (allArity))
import           BDD.BDD                     (pick)
import           BoFun                       (BoFun (isConst, setBit, variables))
import           Data.DecisionDiagram.BDD    (AscOrder, BDD, evaluate, false,
                                              true)
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as IM
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
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

------------------- From BoFun to GenFun -----------------------------

-- Translates a BoFun to a set of the possible GenFuns that it could represent.
-- It is necessary to return a set, as we don't have a way of deciding how
-- the variable oredrings should translate to eachother.
-- Should probably only be used when trying to generate all representations of
-- members of a class.
boFunToGenFuns :: (BoFun f i) => Int -> f -> HashSet GenFun
boFunToGenFuns n f = HS.map (`GenFun` n) bdds
  where
    bdds = boFunToBDDs n f

boFunToBDDs :: (BoFun f i) => Int -> f -> HashSet (BDD AscOrder)
boFunToBDDs n f = case isConst f of
  Just val -> if val then HS.singleton true else HS.singleton false
  Nothing  -> HS.fromList bdds
  where
    bdds = do
      i <- variables f
      let subFunF = setBit (i, False) f
      let subFunT = setBit (i, True) f
      subFunFBDD <- HS.toList $ boFunToBDDs (n - 1) subFunF
      subFunTBDD <- HS.toList $ boFunToBDDs (n - 1) subFunT
      return $ pick n subFunFBDD subFunTBDD

-- Returns the set of all GenFuns that can be represented by the given type.
allRepresentableFuns :: forall f i proxy. (AllArity f, BoFun f i) => proxy f -> Int -> HashSet GenFun
allRepresentableFuns _ n = Set.foldr (HS.union . boFunToGenFuns n) HS.empty reps
  where
    reps :: Set f
    reps = allArity n
