{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Subclasses.General (
  GenFun(GenFun),
  liftBDD,
  falseG,
  trueG,
  notG,
  normalizeGenFun,
  toGenFun,
  allGenFuns,
  majFun,
  iteratedMajFun,
  iteratedFun,
  eval,
  flipInputs,
  generateGenFun
) where
import           Algorithm.Algor           (Algor (..))
import           BDD                       (BDDFun, bddFromOutput, isConstBDD,
                                            normalizeBDD, pick)
import qualified BDD
import           BDD.BDDInstances          ()
import           BoFun                     (BoFun (..), shrinkFun)
import           Data.DecisionDiagram.BDD  (AscOrder, BDD (..), Graph, Sig,
                                            evaluate, false, notB, restrict,
                                            substSet, support, true, var)
import           Data.Function.Memoize     (deriveMemoizable)
import qualified Data.IntMap               as IM
import qualified Data.IntSet               as IS
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Test.QuickCheck           (Arbitrary, Gen, chooseInt, sized,
                                            vector)
import           Test.QuickCheck.Arbitrary (Arbitrary (..), shrink)

-- The internal BDD should only ever be dependent on variables in [1..n]
data GenFun = GenFun (BDD AscOrder) Int
  deriving(Eq, Ord, Show)

$(deriveMemoizable ''GenFun)

-- We normalize the function in setBit, since the normalized function will have the same
-- complexity as the non-normalized one. However, this is only intended for complexity
-- calculations, for a more expected behaviour, simply use restrictGenFun.
instance BoFun GenFun Int where
  isConst :: GenFun -> Maybe Bool
  isConst = liftBDD isConstBDD
  variables :: GenFun -> [Int]
  variables = IS.toList . liftBDD support
  setBit :: (Int, Bool) -> GenFun -> GenFun
  setBit = uncurry setBitNormCanonical

setBit' :: Int -> Bool -> GenFun -> GenFun
setBit' = restrictGenFun

setBitNorm :: Int -> Bool -> GenFun -> GenFun
setBitNorm i v = normalizeGenFun . setBit' i v

setBitNormCanonical :: Int -> Bool -> GenFun -> GenFun
setBitNormCanonical i v = toCanonicForm . setBitNorm i v

restrictGenFun :: Int -> Bool -> GenFun -> GenFun
restrictGenFun i v (GenFun bdd n) = GenFun (restrict i v bdd) (n - 1)

{-
instance Algor GenFun where
  res :: Bool -> GenFun
  res False = falseG
  res True  = trueG
  pic :: Int -> GenFun -> GenFun -> GenFun
  pic n (GenFun gf1 n1) (GenFun gf2 n2) = GenFun (pic n gf1 gf2)
-}

normalizeGenFun :: GenFun -> GenFun
normalizeGenFun (GenFun bdd _) = GenFun bdd' n'
  where
    (bdd', n') = normalizeBDD bdd

-- We have chosen to call a BDD canonical if its leftmost path reaches 0.
-- This is equivalent with the output for an input consistiong only of 0s being 0.
-- Other definitions might be better.
toCanonicForm :: GenFun -> GenFun
toCanonicForm gf@(GenFun bdd n)
  | inCanonicForm bdd = gf
  | otherwise = GenFun (notB bdd) n

inCanonicForm :: BDD AscOrder -> Bool
inCanonicForm = not . evaluate (const False)

------------- QuickCheck ---------------------------

-- Instance for not necessarily normalized GenFuns
instance Arbitrary GenFun where
  arbitrary :: Gen GenFun
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    generateGenFun n'

  shrink :: GenFun -> [GenFun]
  shrink gf = case gf of
    (GenFun (Leaf _) 0) -> []
    (GenFun (Leaf _) n) -> [GenFun (Leaf v') n' | n' <- [0 .. n - 1], v' <- [False, True]]
    _                   -> shrinkFun gf

-- Generator for a General function with at most n variables.
generateGenFun :: Int -> Gen GenFun
generateGenFun n = do
  output <- vector (2^n)
  return $ GenFun (bddFromOutput n output) n

----------------- Boolean operators --------------------------------

{-
mapBDD :: (BDDFun -> BDDFun) -> (GenFun -> GenFun)
mapBDD f  = GenFun . liftBDD f
-}

liftBDD :: (BDDFun -> a) -> (GenFun -> a)
liftBDD f (GenFun gf _) = f gf

falseG :: Int -> GenFun
falseG = GenFun false

trueG :: Int -> GenFun
trueG = GenFun true

notG :: GenFun -> GenFun
notG (GenFun bdd n) = GenFun (notB bdd) n

----------------- Conversions ---------------------------------------

toGenFun :: (BoFun f i) => Int -> f -> GenFun
toGenFun = toGenFun' 1

toGenFun' :: (BoFun f i) => Int -> Int -> f -> GenFun
toGenFun' varN arity f = case isConst f of
  Just False -> falseG arity
  Just True  -> trueG arity
  Nothing -> GenFun (pic varN subBDDF subBDDT) arity
    where
      i = head $ variables f
      (GenFun subBDDF _) = toGenFun' (varN + 1) (arity - 1) $ setBit (i, False) f
      (GenFun subBDDT _) = toGenFun' (varN + 1) (arity - 1) $ setBit (i, True) f

allGenFuns :: Int -> Set GenFun
allGenFuns n
  | n == 0 = Set.fromList [falseG 0, trueG 0]
  | otherwise = Set.fromList [GenFun (pic n a1 a2) n | a1 <- subBDDs, a2 <- subBDDs]
  where
    n' = n - 1
    subFuns = Set.toList $ allGenFuns n'
    subBDDs = map (\(GenFun bdd _) -> bdd) subFuns

----------------- Examples -----------------------------------------

-- n is the number of bits of the functions
majFun' :: Int -> BDDFun
majFun' n = thresholdBDD threshold 1 n
  where
    threshold = (n `div` 2) + 1

thresholdBDD :: Int -> Int -> Int -> BDDFun
thresholdBDD 0 _ _ = true
thresholdBDD threshold i n
  | i > n = false
  | otherwise = pick i
    (thresholdBDD threshold (i + 1) n)
    (thresholdBDD (threshold - 1) (i + 1) n)

iteratedFun' :: Int -> Int -> BDDFun -> BDDFun
iteratedFun' bits levels = iteratedFun'' bits levels 1

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
substituteBase f bits varN = substSet (IM.fromList $ zip [1 .. bits] vars) f
  where
    vars = map var [varN ..]

substituteSubFuns :: BDDFun -> [BDDFun] -> BDDFun
substituteSubFuns f subFuns = substSet (IM.fromList $ zip [1 ..] subFuns) f

--------------- Exported ----------------------------

majFun :: Int -> GenFun
majFun n = GenFun (majFun' n) n

iteratedMajFun :: Int -> Int -> GenFun
iteratedMajFun bits levels = iteratedFun levels (majFun bits)

iteratedFun :: Int -> GenFun -> GenFun
iteratedFun levels (GenFun bdd bits) = GenFun (iteratedFun' bits levels bdd) n
  where
    n = bits ^ levels

----------------- Eval for GenFuns -------------------------

-- Might be better to use an IntMap instead of a list of bools.
eval :: GenFun -> [Bool] -> Maybe Bool
eval gf@(GenFun bdd _) input = isConst $
  foldl (\gf' (varN, v) -> restrictGenFun varN v gf') gf input'
  where
    input' = filter (\(i, _) -> IS.member i supporting) $ zip [1..] input
    supporting = support bdd

flipInputs :: GenFun -> GenFun
flipInputs (GenFun bdd n) = GenFun (BDD.flipInputs bdd) n
