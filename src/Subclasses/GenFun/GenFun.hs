{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
module Subclasses.GenFun.GenFun (
  GenFun(GenFun),
  liftBDD,
  falseG,
  trueG,
  notG,
  constGF,
  toGenFun,
  allGenFuns,
  majFun,
  iteratedMajFun,
  iteratedFun,
  eval,
  generateGenFun,
  flipInputsGenFun,
  prettyShowGenFun,
  prettyPrintGenFun
) where
import           Arity                      (ArbitraryArity (arbitraryArity))
import           BDD.BDD                    (BDDa, allNAryBDDs,
                                             bddFromOutputVector)
import qualified BDD.BDD                    as BDD
import           BDD.BDDInstances           ()
import           Complexity.Algor           (Algor (..))
import           Complexity.BoFun           (BoFun (..), shrinkBoFun)
import           Control.DeepSeq            (NFData)
import           Data.DecisionDiagram.BDD   (AscOrder, BDD (..), false, notB,
                                             restrict, support, true)
import           Data.Function.Memoize      (deriveMemoizable)
import           Data.Hashable              (Hashable)
import qualified Data.IntSet                as IS
import           Data.List.NonEmpty         (NonEmpty ((:|)), append, (<|))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe                 (fromJust)
import           Exploration.PrettyPrinting (PrettyBoFun (prettyShow))
import           GHC.Generics               (Generic)
import           Test.QuickCheck            (Arbitrary, Gen, chooseInt, sized,
                                             vector)
import           Test.QuickCheck.Arbitrary  (Arbitrary (..), shrink)
import           Utils                      (listToVarAssignment)

-- The internal BDD should only ever be dependent on variables in [1..n]
data GenFun = GenFun (BDD AscOrder) Int
  deriving(Eq, Show, Generic, Read)

instance Hashable GenFun

instance NFData GenFun

$(deriveMemoizable ''GenFun)

instance BoFun GenFun Int where
  isConst :: GenFun -> Maybe Bool
  isConst = liftBDD BDD.isConst
  variables :: GenFun -> [Int]
  variables = IS.toList . liftBDD support
  setBit :: (Int, Bool) -> GenFun -> GenFun
  setBit = uncurry restrictGenFun

restrictGenFun :: Int -> Bool -> GenFun -> GenFun
restrictGenFun i v (GenFun bdd n) = GenFun (restrict i v bdd) (n - 1)

------------- QuickCheck ---------------------------

-- Instance for not necessarily minimized GenFuns
instance Arbitrary GenFun where
  arbitrary :: Gen GenFun
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    generateGenFun n'

  shrink :: GenFun -> [GenFun]
  shrink gf = case gf of
    (GenFun (Leaf _) 0) -> []
    (GenFun (Leaf _) n) -> [GenFun (Leaf v') n' | n' <- [0 .. n - 1], v' <- [False, True]]
    _                   -> shrinkBoFun gf

instance ArbitraryArity GenFun where
  arbitraryArity :: Int -> Gen GenFun
  arbitraryArity = generateGenFun

-- Generator for a General function with at most n variables.
generateGenFun :: Int -> Gen GenFun
generateGenFun n = do
  output <- vector (2^n)
  let varAssignment = listToVarAssignment output
  return $ GenFun (bddFromOutputVector n varAssignment) n

----------------- Boolean operators --------------------------------

liftBDD :: (BDDa -> a) -> (GenFun -> a)
liftBDD f (GenFun gf _) = f gf

falseG :: Int -> GenFun
falseG = GenFun false

trueG :: Int -> GenFun
trueG = GenFun true

notG :: GenFun -> GenFun
notG (GenFun bdd n) = GenFun (notB bdd) n

constGF :: Bool -> Int -> GenFun
constGF v = GenFun v'
  where
    v' = if v then true else false

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

{-
allGenFunReps :: Int -> HashSet GenFun
allGenFunReps = (map allGenFunReps' [0 ..] !!)
  where
    allGenFunReps' n
      | n == 0 = HS.fromList [falseG 0, trueG 0]
      | otherwise = HS.fromList [GenFun (pic n a1 a2) n | a1 <- subBDDs, a2 <- subBDDs]
      where
        n' = n - 1
        subFuns = HS.toList $ allGenFunReps n'
        subBDDs = map (\(GenFun bdd _) -> bdd) subFuns-}

allGenFuns :: Int -> [GenFun]
allGenFuns n = map (`GenFun` n) $ allNAryBDDs n

--------------- Examples ----------------------------

majFun :: Int -> GenFun
majFun n = GenFun (BDD.majFun n) n

iteratedMajFun :: Int -> Int -> GenFun
iteratedMajFun bits levels = iteratedFun levels (majFun bits)

-- Note that this function does not use Iterated. This is because we want to keep
-- the result as a GenFun to enjoy the benefits that the BDD representation gives us.
iteratedFun :: Int -> GenFun -> GenFun
iteratedFun levels (GenFun bdd bits)
  | levels <= 0 = error "Number of levels must be >= 1"
  | bits <= 0 = error "Number of input bits must be >= 1"
  | otherwise = GenFun (BDD.iteratedFun bits levels bdd) n
  where
    n = bits ^ levels

----------------- Eval for GenFuns -------------------------

eval :: GenFun -> [Bool] -> Maybe Bool
eval gf input = isConst $
  foldl (\gf' (varN, v) -> setBit (varN, v) gf') gf input'
  where
    input' = zip [1..] input

---------------- Utils ------------------------------------

flipInputsGenFun :: GenFun -> GenFun
flipInputsGenFun (GenFun bdd n) = GenFun (BDD.flipInputs bdd) n

prettyPrintGenFun :: GenFun -> IO ()
prettyPrintGenFun = putStrLn . prettyShowGenFun

prettyShowGenFun :: GenFun -> String
prettyShowGenFun = unlines . map showRow . NE.toList . constructTruthTable

showRow :: NonEmpty Bool -> String
showRow (v :| vs) = case vs of
  []         -> " | " ++ showBool v
  (v' : vs') -> showBool v ++ showRow (v' :| vs')
  where
    showBool False = "0"
    showBool True  = "1"

constructTruthTable :: GenFun -> NonEmpty (NonEmpty Bool)
constructTruthTable = constructTruthTable' 1

constructTruthTable' :: Int -> GenFun -> NonEmpty (NonEmpty Bool)
constructTruthTable' _ (GenFun bdd 0) = (fromJust (BDD.isConst bdd) :| []) :| []
constructTruthTable' varN gf = append zeroTable oneTable
  where
    zeroTable = subTable False
    oneTable = subTable True
    subTable v = NE.map (v <|) $ constructTruthTable' (varN + 1) $ setBit (varN, v) gf

instance PrettyBoFun GenFun where
  prettyShow :: GenFun -> String
  prettyShow = prettyShowGenFun
