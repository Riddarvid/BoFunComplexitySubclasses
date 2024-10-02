{-# LANGUAGE InstanceSigs #-}
module Properties (
  propComplexityNot,
  propNormalizedEqual,
  propNormalizedEval,
  propFlipCorrect,
  propFlipOutput,
  propFlipAllInputs,
  propCorrectComplexity
) where
import           Algorithm.GenAlg         (genAlgThinMemoPoly)
import           Algorithm.GenAlgPW       (computeMin)
import           BDD                      (BDDFun, flipInputs, normalizeBDD)
import           BDD.BDDInstances         ()
import           Data.DecisionDiagram.BDD (evaluate, false, notB, support, true,
                                           var, xor, (.&&.), (.<=>.), (.=>.),
                                           (.||.))
import qualified Data.IntSet              as IS
import           Data.Maybe               (fromJust)
import           Data.Ratio               ((%))
import qualified Data.Set                 as Set
import           Poly.PiecewisePoly       (minPWs, pieces, piecewiseFromPoly,
                                           propIsMirrorPW)
import           Subclasses.General       (GenFun, mapBDD, notG)
import           Test.QuickCheck          (Arbitrary (arbitrary), Property,
                                           choose, elements, oneof, sized,
                                           vector, vectorOf, (=/=), (===))
import           Test.QuickCheck.Gen      (Gen)

-- Currently becomes very slow with more than 5 bits, so the arbitrary instance
-- for BDD funs is limited to max 5 bits.
propComplexityNot :: GenFun -> Property
propComplexityNot f = computeMin f === computeMin f'
  where
    f' = notG f

-- Represents non-variable-normalized BDDs, only to be used with QuickCheck.
newtype BDDFun' = BDDFun' BDDFun
  deriving(Eq, Show)

instance Arbitrary BDDFun' where
  arbitrary :: Gen BDDFun'
  arbitrary = sized genBDDFun'

genBDDFun' :: Int -> Gen BDDFun'
genBDDFun' n = do
  n' <- choose (0, n)
  genBDDFun'' n'

-- This should be able to create all functions as we have access to AND, OR, and NOT (true xor)
genBDDFun'' :: Int -> Gen BDDFun'
genBDDFun'' 0 = elements [BDDFun' false, BDDFun' true]
genBDDFun'' n = do
  vars <- vectorOf n genVarOrConst
  ops <- vectorOf (n - 1) genOp
  return $ BDDFun' $ combine vars ops

genVarOrConst :: Gen BDDFun
genVarOrConst = oneof [var <$> arbitrary, return false, return true]

genOp :: Gen (BDDFun -> BDDFun -> BDDFun)
genOp = elements [(.&&.), (.||.), xor, (.=>.), (.<=>.)]

combine :: [a] -> [a -> a -> a] -> a
combine [] _                = undefined
combine [v] []              = v
combine (v : vs) (op : ops) = v `op` combine vs ops
combine _ _                 = error "Illegal combination of vars and ops"

-- Checks that the variable set is correct after normalization.
propNormalizedEqual :: BDDFun' -> Property
propNormalizedEqual (BDDFun' bdd) = vars === [0 .. n - 1]
  where
    vars = IS.toAscList $ support $ normalizeBDD bdd
    n = IS.size $ support bdd

data BDDAndInput = BDDAndInput BDDFun' [Bool]
  deriving(Show)

instance Arbitrary BDDAndInput where
  arbitrary :: Gen BDDAndInput
  arbitrary = sized $ \n -> do
    bdd <- genBDDFun'' n
    input <- vector n
    return $ BDDAndInput bdd input

evalBDDFun :: BDDFun -> [Bool] -> Bool
evalBDDFun bdd vals = evaluate evalF bdd
  where
    vars = IS.toAscList $ support bdd
    evalF i = fromJust $ lookup i $ zip vars vals

propNormalizedEval :: BDDAndInput -> Property
propNormalizedEval (BDDAndInput (BDDFun' bdd) input) = eval' bdd === eval' (normalizeBDD bdd)
  where
    eval' f = evalBDDFun f input

----------------- Complexity properties --------------------------------

propFlipCorrect :: BDDAndInput -> Property
propFlipCorrect (BDDAndInput (BDDFun' bdd) input) = eval' bdd =/= eval' (notB bdd)
  where
    eval' f = evalBDDFun f input

propFlipOutput :: GenFun -> Property
propFlipOutput gf = computeMin gf === computeMin (notG gf)

propFlipAllInputs :: GenFun -> Property
propFlipAllInputs gf = propIsMirrorPW (1 % 2)
  (computeMin gf)
  (computeMin (mapBDD flipInputs gf))

-------------------- computeMin ----------------------------------

propCorrectComplexity :: GenFun -> Property
propCorrectComplexity gf =
  pieces (computeMin gf) ===
  pieces (minPWs $ map piecewiseFromPoly $ Set.toList $ genAlgThinMemoPoly gf)
