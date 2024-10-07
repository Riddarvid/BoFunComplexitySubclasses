{-# LANGUAGE InstanceSigs #-}
module Properties (
  propComplexityNot,
  propNormalizedCorrectVars,
  propNormalizedComplexity,
  propFlipCorrect,
  propFlipOutput,
  propFlipAllInputs,
  propCorrectComplexity
) where
import           Algorithm.GenAlg    (genAlgThinMemoPoly)
import           Algorithm.GenAlgPW  (computeMin)
import           BDD.BDDInstances    ()
import           BoFun               (BoFun (variables))
import           Data.List           (sort)
import           Data.Ratio          ((%))
import qualified Data.Set            as Set
import           Poly.PiecewisePoly  (minPWs, pieces, piecewiseFromPoly,
                                      propIsMirrorPW)
import           Subclasses.General  (GenFun (GenFun), eval, flipInputs,
                                      generateGenFun, normalizeGenFun, notG)
import           Test.QuickCheck     (Arbitrary (arbitrary), Property, sized,
                                      vector, (=/=), (===))
import           Test.QuickCheck.Gen (Gen)

-- Currently becomes very slow with more than 5 bits, so the arbitrary instance
-- for BDD funs is limited to max 5 bits.
propComplexityNot :: GenFun -> Property
propComplexityNot f = computeMin f === computeMin f'
  where
    f' = notG f

-- Checks that the variable set is correct after normalization.
propNormalizedCorrectVars :: GenFun -> Property
propNormalizedCorrectVars gf = (vars, arity)  === ([1 .. n], n)
  where
    gf'@(GenFun _ arity) = normalizeGenFun gf
    vars = sort $ variables gf'
    n = length $ variables gf

propNormalizedComplexity :: GenFun -> Property
propNormalizedComplexity gf = computeMin gf === computeMin (normalizeGenFun gf)

data GenFunAndInput = GenFunAndInput GenFun [Bool]
  deriving(Show)

instance Arbitrary GenFunAndInput where
  arbitrary :: Gen GenFunAndInput
  arbitrary = sized $ \n -> do
    gf <- generateGenFun n
    input <- vector n
    return $ GenFunAndInput gf input

----------------- Complexity properties --------------------------------

propFlipCorrect :: GenFunAndInput -> Property
propFlipCorrect (GenFunAndInput gf input) = eval gf input =/= eval (notG gf) input

propFlipOutput :: GenFun -> Property
propFlipOutput gf = computeMin gf === computeMin (notG gf)

propFlipAllInputs :: GenFun -> Property
propFlipAllInputs gf = propIsMirrorPW (1 % 2)
  (computeMin gf)
  (computeMin (flipInputs gf))

-------------------- computeMin ----------------------------------

propCorrectComplexity :: GenFun -> Property
propCorrectComplexity gf =
  pieces (computeMin gf) ===
  pieces (minPWs $ map piecewiseFromPoly $ Set.toList $ genAlgThinMemoPoly gf)
