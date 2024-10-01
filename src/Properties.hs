{-# LANGUAGE InstanceSigs #-}
module Properties (
  propComplexityNot,
  propNormalizedEqual,
  propNormalizedEval,
  propFlipCorrect,
  propFlipOutput,
  propFlipAllInputs
) where
import           Algorithm.GenAlgPW       (computeMin)
import           BDD                      (BDDFun, flipInputs, normalizeBDD)
import           BDD.BDDInstances         ()
import           Data.DecisionDiagram.BDD (evaluate, false, notB, support, true,
                                           var, xor, (.&&.), (.<=>.), (.=>.),
                                           (.||.))
import qualified Data.IntSet              as IS
import           Data.Maybe               (fromJust)
import           Data.Ratio               ((%))
import           Poly.PiecewisePoly       (propIsMirrorPW)
import           Test.QuickCheck          (Arbitrary (arbitrary), Property,
                                           choose, elements, oneof, resize,
                                           sized, vector, vectorOf, (=/=),
                                           (===))
import           Test.QuickCheck.Gen      (Gen)

-- Currently becomes very slow with more than 5 bits, so the arbitrary instance
-- for BDD funs is limited to max 5 bits.
propComplexityNot :: BDDFun -> Property
propComplexityNot f = computeMin f === computeMin f'
  where
    f' = notB f

-- Represents non-variable-normalized BDDs, only to be used with QuickCheck.
newtype BDDFun' = BDDFun' BDDFun
  deriving(Eq, Show)

instance Arbitrary BDDFun' where
  arbitrary :: Gen BDDFun'
  arbitrary = resize 10 $ sized genBDDFun'

genBDDFun' :: Int -> Gen BDDFun'
genBDDFun' n = do
  n' <- choose (1, n)
  genBDDFun'' n'

-- This should be able to create all functions as we have access to AND, OR, and NOT (true xor)
genBDDFun'' :: Int -> Gen BDDFun'
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
  arbitrary = resize 10 $ sized $ \n -> do
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

propFlipOutput :: BDDFun -> Property
propFlipOutput bdd = computeMin bdd === computeMin (notB bdd)

propFlipAllInputs :: BDDFun -> Property
propFlipAllInputs bdd = propIsMirrorPW (1 % 2)
  (computeMin bdd)
  (computeMin (flipInputs bdd))


