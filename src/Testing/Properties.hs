{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Testing.Properties (
  propNormalizedCorrectVars,
  propNormalizedComplexity,
  propFlipOutputCorrect,
  propFlipOutputComplexity,
  propFlipInputComplexity,
  propComputeMinCorrect,
  propConversionSymm,
  propComputeMin'Correct,
  propRepsCorrect,
  propRationalSign,
  IterInput
) where
import           Algebraic                    (signAtAlgebraic, signAtRational,
                                               toAlgebraic)
import           Algorithm.GenAlg             (genAlgThinMemoPoly)
import           Algorithm.GenAlgPW           (computeMin, computeMin')
import           BDD.BDDInstances             ()
import           BoFun                        (BoFun (variables))
import           Data.List                    (sort)
import           Data.Ratio                   ((%))
import qualified Data.Set                     as Set
import           DSLsofMath.PSDS              (Poly)
import           Exploration.Eval             (evalSymmetric)
import           Exploration.Translations     (genToBasicSymmetricNaive)
import           Poly.PiecewisePoly           (minPWs, pieces,
                                               piecewiseFromPoly,
                                               propIsMirrorPW)
import qualified Subclasses.GenFun            as Gen
import           Subclasses.GenFun            (GenFun, eval, flipInputsGenFun,
                                               generateGenFun, notG, toGenFun)
import           Subclasses.Id                ()
import           Subclasses.IteratedInstances ()
import           Subclasses.NormalizedGenFun  (mkNGF, ngfArity)
import qualified Subclasses.Symmetric         as Symm
import           Subclasses.Symmetric         (SymmetricFun)
import qualified Subclasses.Threshold         as Thresh
import           Test.QuickCheck              (Arbitrary (arbitrary, shrink),
                                               Property, chooseInt, conjoin,
                                               elements, sized, vector, (=/=),
                                               (===))
import           Test.QuickCheck.Gen          (Gen)

----------------- Types --------------------------------------

-- Type representing an n-bit GenFun and an n-bit input
data GenFunAndInput = GenFunAndInput GenFun [Bool]
  deriving(Show)

instance Arbitrary GenFunAndInput where
  arbitrary :: Gen GenFunAndInput
  arbitrary = sized $ \n -> do
    gf <- generateGenFun n
    input <- vector n
    return $ GenFunAndInput gf input

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

----------------- Inverted input/output ---------------------------

-- ! . f should not give the same result as f
propFlipOutputCorrect :: GenFunAndInput -> Property
propFlipOutputCorrect (GenFunAndInput gf input) = eval gf input =/= eval (notG gf) input

-- The complexity should not change when inverting the output
propFlipOutputComplexity :: GenFun -> Property
propFlipOutputComplexity gf = computeMin gf === computeMin (notG gf)

-- The complexity should be mirrored in x=1/2 when inverting the inputs
propFlipInputComplexity :: GenFun -> Property
propFlipInputComplexity gf = propIsMirrorPW (1 % 2)
  (computeMin gf)
  (computeMin (flipInputsGenFun gf))

------------------- Normalization ------------------------------------

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

-------------------- Algorithms ----------------------------------
-- Properties comparing the correctness of the complexity algorithms

-- computeMin and genAlg should yield equivalent PWs for the same function
propComputeMinCorrect :: GenFun -> Property
propComputeMinCorrect gf =
  pieces (computeMin gf) ===
  pieces (minPWs $ map piecewiseFromPoly $ Set.toList $ genAlgThinMemoPoly gf)

-- computeMin and computeMin' should yield the same result for the same function
propComputeMin'Correct :: GenFun -> Property
propComputeMin'Correct gf = computeMin gf === computeMin' gf

------------------- Conversions ----------------------------

-- Converting from BasicSymm to GenFun and back again should yield the original function
propConversionSymm :: SymmetricFun -> Property
propConversionSymm f = Just f === genToBasicSymmetricNaive (toGenFun (Symm.arity f) f)

-- Converting from Iterated ThresholdFun to GenFun and back again should yield a set
-- of possible representations which includes the original function
-- TODO-NEW: Figure out if we actually want this test or not. Right now it basically
-- compares the generateAll and generateOne implementations.
{-propConversionIteratedThreshold :: Iterated ThresholdFun -> Property
propConversionIteratedThreshold f = property $ elem f funs
  where
    arity = arityIteratedThreshold f
    funs = genToIteratedThresholdFun (toGenFun arity f)-}

----------------------- Representations ---------------------------------------

-- The Gen, Symm, and Thresh representations of a flat majority function should
-- all yield the same result for the same input.
propRepsCorrect :: MajInput -> Property
propRepsCorrect (Input vals) = conjoin
  [
    resSymm === resGen,
    resThresh === resGen
  ]
  where
    n = length vals
    nOnes = length $ filter id vals
    majGeneral = Gen.majFun n
    majSymm = Symm.majFun n
    majThreshold = Thresh.majFun n
    resSymm = evalSymmetric majSymm nOnes
    resGen = evalSymmetric majGeneral nOnes
    resThresh = evalSymmetric majThreshold nOnes

data IterInput = IterInput Int Int
  deriving (Show)

-- The total number of bits will never be higher than n.
instance Arbitrary IterInput where
  arbitrary :: Gen IterInput
  arbitrary = sized $ \limit -> do
    let limit' = if limit == 0 then 1 else limit
    elements [IterInput bits levels | bits <- [1 .. limit'], levels <- validLevels bits limit']

validLevels :: Int -> Int -> [Int]
validLevels bits limit = takeWhile (\levels -> bits ^ levels <= limit) [1 .. limit]

newtype IterMajInput = IMI IterInput
  deriving (Show)

instance Arbitrary IterMajInput where
  arbitrary :: Gen IterMajInput
  arbitrary = do
    IterInput bits levels <- arbitrary
    let bits' = if even bits then bits - 1 else bits
    return $ IMI $ IterInput bits' levels

-- Static test that ensures that the Gen, Symm, and Thresh representations of
-- maj 3 2 yield the same complexity.
-- This property is obsolete since we show
-- propRepsCorrect and propComputeMinCorrect + propComputeMin'Correct
{-propRepsComplexity :: IterMajInput -> Property
propRepsComplexity (IMI (IterInput bits levels)) = conjoin
  [
    symm === gen,
    thresh === gen
  ]
  where
    gen = computeMin $ Gen.iteratedMajFun bits levels
    symm = computeMin $ Symm.iteratedMajFun bits levels
    thresh = computeMin $ Thresh.iteratedMajFun bits levels-}

------------------ Algebraic numbers --------------------------------

-- The sign of a polynomial at a rational point should not depend on whether r is
-- expressed as a rational number or as an algebraic number.
propRationalSign :: Rational -> Poly Rational -> Property
propRationalSign r p = s1 === s2
  where
    x = toAlgebraic r
    s1 = signAtRational r p
    s2 = signAtAlgebraic x p
