{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Testing.Properties (
  propComplexityNot,
  propNormalizedCorrectVars,
  propNormalizedComplexity,
  propFlipCorrect,
  propFlipOutput,
  propFlipAllInputs,
  propCorrectComplexity,
  propConversionSymm,
  propConversionIteratedThreshold,
  propComputeMin'Correct,
  propMajEqual,
  propSameComplexity,
  propRationalSign
) where
import           Algebraic                   (signAtAlgebraic, signAtDyadic,
                                              toAlgebraic)
import           Algorithm.GenAlg            (genAlgThinMemoPoly)
import           Algorithm.GenAlgPW          (computeMin, computeMin')
import           BDD.BDDInstances            ()
import           BoFun                       (BoFun (variables))
import           Data.List                   (sort)
import           Data.Ratio                  ((%))
import qualified Data.Set                    as Set
import           DSLsofMath.PSDS             (Poly)
import           Exploration.Translations    (genToBasicSymmetricNaive,
                                              genToIteratedThresholdFun)
import           Poly.PiecewisePoly          (minPWs, pieces, piecewiseFromPoly,
                                              propIsMirrorPW)
import qualified Subclasses.GenFun           as Gen
import           Subclasses.GenFun           (GenFun, eval, flipInputsGenFun,
                                              generateGenFun, notG, toGenFun)
import           Subclasses.Iterated         (Iterated)
import           Subclasses.NormalizedGenFun (mkNGF, ngfArity)
import qualified Subclasses.Symmetric        as Symm
import           Subclasses.Symmetric        (BasicSymmetric (BasicSymmetric))
import qualified Subclasses.Threshold        as Thresh
import           Subclasses.Threshold        (ThresholdFun,
                                              arityIteratedThreshold)
import           Test.QuickCheck             (Arbitrary (arbitrary, shrink),
                                              Property, Testable (property),
                                              chooseInt, conjoin, sized, vector,
                                              (=/=), (===))
import           Test.QuickCheck.Gen         (Gen)

----------------- Inverted input/output ---------------------------

-- The complexity of a function should not change if we invert the output
propComplexityNot :: GenFun -> Property
propComplexityNot f = computeMin f === computeMin f'
  where
    f' = notG f

-- ! . f should not give the same result as f
propFlipCorrect :: GenFunAndInput -> Property
propFlipCorrect (GenFunAndInput gf input) = eval gf input =/= eval (notG gf) input

-- The complexity should not change when inverting the output
propFlipOutput :: GenFun -> Property
propFlipOutput gf = computeMin gf === computeMin (notG gf)

-- The complexity should be mirrored in x=1/2 when inverting the inputs
propFlipAllInputs :: GenFun -> Property
propFlipAllInputs gf = propIsMirrorPW (1 % 2)
  (computeMin gf)
  (computeMin (flipInputsGenFun gf))


-- After normalizing a GenFun, it should have the variables [1 .. n], where
-- n is the number of variables in the original GenFun.
propNormalizedCorrectVars :: GenFun -> Property
propNormalizedCorrectVars gf = (vars, arity)  === ([1 .. n], n)
  where
    ngf = mkNGF gf
    vars = sort $ variables ngf
    arity = ngfArity ngf
    n = length $ variables gf

-- Normalization should not affect the complexity of a GenFun
propNormalizedComplexity :: GenFun -> Property
propNormalizedComplexity gf = computeMin gf === computeMin (mkNGF gf)

-- Type representing an n-bit GenFun and an n-bit input
data GenFunAndInput = GenFunAndInput GenFun [Bool]
  deriving(Show)

instance Arbitrary GenFunAndInput where
  arbitrary :: Gen GenFunAndInput
  arbitrary = sized $ \n -> do
    gf <- generateGenFun n
    input <- vector n
    return $ GenFunAndInput gf input

----------------- Complexity properties --------------------------------



-------------------- computeMin ----------------------------------

-- computeMin and genAlg should yield equivalent PWs for the same function
propCorrectComplexity :: GenFun -> Property
propCorrectComplexity gf =
  pieces (computeMin gf) ===
  pieces (minPWs $ map piecewiseFromPoly $ Set.toList $ genAlgThinMemoPoly gf)

------------------- conversions ----------------------------

-- Converting from BasicSymm to GenFun and back again should yield the original function
propConversionSymm :: BasicSymmetric -> Property
propConversionSymm f@(BasicSymmetric rv) = Just f === genToBasicSymmetricNaive (toGenFun arity f)
  where
    arity = length rv - 1

-- Converting from Iterated ThresholdFun to GenFun and back again should yield a set
-- of possible representations which includes the original function
propConversionIteratedThreshold :: Iterated ThresholdFun -> Property
propConversionIteratedThreshold f = property $ f `elem` genToIteratedThresholdFun (toGenFun arity f)
  where
    arity = arityIteratedThreshold f

-- computeMin and computeMin' should yield the same result for the same function
propComputeMin'Correct :: GenFun -> Property
propComputeMin'Correct gf = computeMin gf === computeMin' gf

-- Type representing the input to a majority function. Should always have odd length.
newtype MajInput = Input [Bool]
  deriving (Show)

instance Arbitrary MajInput where
  arbitrary :: Gen MajInput
  arbitrary = sized $ \n -> do
    n' <- chooseInt (1, n)
    let n'' = if even n' then n' + 1 else n'
    Input <$> vector n''
  shrink :: MajInput -> [MajInput]
  shrink (Input [False]) = []
  shrink (Input [True]) = [Input [False]]
  shrink (Input vals) = [Input [False], Input [True]] ++ shorter ++ map Input (changeOneToFalse vals)
    where
      changeOneToFalse []           = []
      changeOneToFalse (True : xs)  = (False : xs) : map (True :) (changeOneToFalse xs)
      changeOneToFalse (False : xs) = map (False :) (changeOneToFalse xs)

      shorter = if length vals > 1 then [Input (drop 2 vals)] else []

-- The Gen, Symm, and Thresh representations of a flat majority function should
-- all yield the same result for the same input.
propMajEqual :: MajInput -> Property
propMajEqual (Input vals) = conjoin
  [
    resSymm === resGen,
    resThresh === resGen
  ]
  where
    n = length vals
    majGeneral = Gen.majFun n
    majSymm = toGenFun n $ Symm.majFun n
    majThreshold = toGenFun n $ Thresh.majFun n
    resSymm = eval majSymm vals
    resGen = eval majGeneral vals
    resThresh = eval majThreshold vals

-- Static test that ensures that the Gen, Symm, and Thresh representations of
-- maj 3 2 yield the same complexity.
propSameComplexity :: Property
propSameComplexity = conjoin
  [
    symm === gen,
    thresh === gen
  ]
  where
    gen = computeMin $ Gen.iteratedMajFun 3 2
    symm = computeMin $ Symm.iteratedMajFun 3 2
    thresh = computeMin $ Thresh.iteratedMajFun 3 2

-- The sign of a polynomial at a rational point should not depend on whether r is
-- expressed as a rational number or as an algebraic number.
propRationalSign :: Rational -> Poly Rational -> Property
propRationalSign r p = s1 === s2
  where
    x = toAlgebraic r
    s1 = signAtDyadic r p
    s2 = signAtAlgebraic x p
