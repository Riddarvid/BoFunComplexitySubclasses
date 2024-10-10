{-# LANGUAGE InstanceSigs #-}
module Properties (
  propComplexityNot,
  propNormalizedCorrectVars,
  propNormalizedComplexity,
  propFlipCorrect,
  propFlipOutput,
  propFlipAllInputs,
  propCorrectComplexity,
  propConversionSymm,
  propConversionIteratedThreshold,
  propComputeMin'Correct
) where
import           Algorithm.GenAlg            (genAlgThinMemoPoly)
import           Algorithm.GenAlgPW          (computeMin, computeMin')
import           BDD.BDDInstances            ()
import           BoFun                       (BoFun (variables))
import           Data.List                   (sort)
import           Data.Ratio                  ((%))
import qualified Data.Set                    as Set
import           Poly.PiecewisePoly          (minPWs, pieces, piecewiseFromPoly,
                                              propIsMirrorPW)
import           Subclasses.General          (GenFun, eval, flipInputsGenFun,
                                              generateGenFun, notG, toGenFun)
import           Subclasses.Iterated         (Iterated)
import           Subclasses.NormalizedGenFun (mkNGF, ngfArity)
import           Subclasses.Symmetric        (BasicSymmetric (BasicSymmetric))
import           Subclasses.Threshold        (ThresholdFun,
                                              arityIteratedThreshold)
import           Test.QuickCheck             (Arbitrary (arbitrary), Property,
                                              Testable (property), sized,
                                              vector, (=/=), (===))
import           Test.QuickCheck.Gen         (Gen)
import           Translations                (genToBasicSymmetricNaive,
                                              genToIteratedThresholdFun)

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
    ngf = mkNGF gf
    vars = sort $ variables ngf
    arity = ngfArity ngf
    n = length $ variables gf

propNormalizedComplexity :: GenFun -> Property
propNormalizedComplexity gf = computeMin gf === computeMin (mkNGF gf)

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
  (computeMin (flipInputsGenFun gf))

-------------------- computeMin ----------------------------------

propCorrectComplexity :: GenFun -> Property
propCorrectComplexity gf =
  pieces (computeMin gf) ===
  pieces (minPWs $ map piecewiseFromPoly $ Set.toList $ genAlgThinMemoPoly gf)

------------------- conversions ----------------------------

propConversionSymm :: BasicSymmetric -> Property
propConversionSymm f@(BasicSymmetric rv) = Just f === genToBasicSymmetricNaive (toGenFun arity f)
  where
    arity = length rv - 1

propConversionIteratedThreshold :: Iterated ThresholdFun -> Property
propConversionIteratedThreshold f = property $ f `elem` genToIteratedThresholdFun (toGenFun arity f)
  where
    arity = arityIteratedThreshold f

propComputeMin'Correct :: GenFun -> Property
propComputeMin'Correct gf = computeMin gf === computeMin' gf
