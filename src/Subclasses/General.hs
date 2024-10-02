{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Subclasses.General (
  GenFun(GenFun),
  liftBDD,
  mapBDD,
  falseG,
  trueG,
  notG,
  toGenFun,
  allGenFuns,
  majFun,
  iteratedMajFun,
  iteratedFun,
  eval
) where
import           Algorithm.Algor           (Algor (..), allAlgors)
import           Algorithm.GenAlg          (funToAlg)
import           BDD                       (BDDFun, bddFromOutput, isConstBDD,
                                            normalizeBDD, pick)
import           BDD.BDDInstances          ()
import           BoFun                     (BoFun (..), shrinkFun)
import           Data.DecisionDiagram.BDD  (AscOrder, BDD (..), false, notB,
                                            restrict, substSet, support, true,
                                            var)
import           Data.Function.Memoize     (deriveMemoizable)
import qualified Data.IntMap               as IM
import qualified Data.IntSet               as IS
import           Data.Set                  (Set)
import           Test.QuickCheck           (Arbitrary, Gen, chooseInt, sized,
                                            vector)
import           Test.QuickCheck.Arbitrary (Arbitrary (..), shrink)

-- The internal BDD should only ever contain the variables 0..n-1
newtype GenFun = GenFun (BDD AscOrder)
  deriving(Eq, Ord, Show)

$(deriveMemoizable ''GenFun)

instance BoFun GenFun Int where
  isConst :: GenFun -> Maybe Bool
  isConst = liftBDD isConstBDD
  variables :: GenFun -> [Int]
  variables = IS.toList . liftBDD support
  setBit :: (Int, Bool) -> GenFun -> GenFun
  setBit (i, v) = mapBDD (normalizeBDD . restrict i v)

instance Algor GenFun where
  res :: Bool -> GenFun
  res False = falseG
  res True  = trueG
  pic :: Int -> GenFun -> GenFun -> GenFun
  pic n (GenFun gf1) (GenFun gf2) = GenFun $ pic n gf1 gf2

normalizeGenFun :: GenFun -> GenFun
normalizeGenFun = mapBDD normalizeBDD

------------- QuickCheck ---------------------------

instance Arbitrary GenFun where
  arbitrary :: Gen GenFun
  arbitrary = sized genFun

  shrink :: GenFun -> [GenFun]
  shrink gf
    | gf /= gf' = shrink gf'
    where
      gf' = normalizeGenFun gf
  shrink gf = case gf of
    (GenFun (Leaf v)) -> if v then [falseG] else []
    _                 -> shrinkFun gf

-- Generator for a General function with at most n variables.
genFun :: Int -> Gen GenFun
genFun n' = do
  n <- chooseInt (0, n')
  output <- vector (2^n)
  return $ GenFun $ bddFromOutput n output

----------------- Generating all general functions -----------------

allGenFuns :: Int -> Set GenFun
allGenFuns = allAlgors

----------------- Boolean operators --------------------------------

mapBDD :: (BDDFun -> BDDFun) -> (GenFun -> GenFun)
mapBDD f  = GenFun . liftBDD f

liftBDD :: (BDDFun -> a) -> (GenFun -> a)
liftBDD f (GenFun gf) = f gf

falseG :: GenFun
falseG = GenFun false

trueG :: GenFun
trueG = GenFun true

notG :: GenFun -> GenFun
notG = mapBDD notB

----------------- Conversions ---------------------------------------

toGenFun :: (BoFun f i) => f -> GenFun
toGenFun = funToAlg

----------------- Examples -----------------------------------------

majFun' :: Int -> BDDFun
majFun' n = thresholdBDD threshold 0 n
  where
    threshold = (n `div` 2) + 1

thresholdBDD :: Int -> Int -> Int -> BDDFun
thresholdBDD 0 _ _ = true
thresholdBDD threshold i n
  | i >= n = false
  | otherwise = pick i
    (thresholdBDD threshold (i + 1) n)
    (thresholdBDD (threshold - 1) (i + 1) n)

iteratedFun' :: Int -> Int -> BDDFun -> BDDFun
iteratedFun' bits levels = iteratedFun'' bits levels 0

iteratedFun'' :: Int -> Int -> Int -> BDDFun -> BDDFun
iteratedFun'' bits _levels _varN f = go (_levels - 1) _varN
  where
    go :: Int -> Int -> BDDFun
    go levels varN
      | levels == 0 = substituteBase f bits varN
      | otherwise = substituteSubFuns f subFuns
      where
        varFactor = bits * levels
        factors = iterate (+ varFactor) varN
        subFuns = map (go (levels - 1)) $ take bits factors

substituteBase :: BDDFun -> Int -> Int -> BDDFun
substituteBase f bits varN = substSet (IM.fromList $ zip [0 .. bits - 1] vars) f
  where
    vars = map var [varN ..]

substituteSubFuns :: BDDFun -> [BDDFun] -> BDDFun
substituteSubFuns f subFuns = substSet (IM.fromList $ zip [0 ..] subFuns) f

iteratedMajFun' :: Int -> Int -> BDDFun
iteratedMajFun' bits levels = iteratedFun' bits levels (majFun' bits)

--------------- Exported ----------------------------

majFun :: Int -> GenFun
majFun = GenFun . majFun'

iteratedMajFun :: Int -> Int -> GenFun
iteratedMajFun bits = GenFun . iteratedMajFun' bits

iteratedFun :: Int -> Int -> BDDFun -> GenFun
iteratedFun bits levels = GenFun . iteratedFun' bits levels

----------------- Eval for GenFuns -------------------------

eval :: GenFun -> [Bool] -> Maybe Bool
eval gf input = case isConst gf of
  Just val -> Just val
  Nothing -> case variables gf of
    [] -> error "Function is not const but has no variables"
    (v : _) -> if length input >= v
      then Nothing
      else eval (setBit (v, input !! v) gf) input
