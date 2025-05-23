{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Testing.Properties (
  CritLimitInput,
  propMinimizedCorrectVars,
  propMinimizedComplexity,
  propFlipOutputCorrect,
  propFlipOutputComplexity,
  propFlipInputComplexity,
  propComputeMinCorrect,
  propConversionSymm,
  propComputeMin'Correct,
  propRepsCorrect,
  propIterRepsCorrect,
  propRationalSign,
  propAlgebraicTranslation,
  propMaxNumCritical,
  propCriticalSwitches,
  propKnownCrits,
  propIteratedNoLoop
) where
import           Algebraic                                (Algebraic (Rational),
                                                           signAtAlgebraic,
                                                           signAtRational,
                                                           toAlgebraic,
                                                           translateRational)
import           BDD.BDDInstances                         ()
import           Complexity.BoFun                         (BoFun (variables))
import           Complexity.GenAlg                        (genAlgThinMemoPoly)
import           Complexity.Piecewise                     (complexity,
                                                           explicitComplexity)
import           Data.List                                (isInfixOf, sort)
import           Data.Ratio                               ((%))
import qualified Data.Set                                 as Set
import           DSLsofMath.PSDS                          (Poly (P), degree)
import           Exploration.Critical                     (Critical (Maximum, Minimum),
                                                           criticalPointsInPiece,
                                                           determineUncertain)
import           Exploration.Eval                         (evalNonSymmetric,
                                                           evalSymmetric)
import           Exploration.Translations                 (genToBasicSymmetricNaive)
import           Poly.PiecewisePoly                       (minPWs, pieces,
                                                           piecewiseFromPoly,
                                                           propIsMirrorPW)
import           Poly.PolynomialExtra                     (genNonZeroPoly)
import qualified Subclasses.GenFun.GenFun                 as Gen
import           Subclasses.GenFun.GenFun                 (GenFun, eval,
                                                           flipInputsGenFun,
                                                           generateGenFun, notG,
                                                           toGenFun)
import           Subclasses.GenFun.MinimizedGenFun        (mkNGF, ngfArity)
import           Subclasses.MultiComposed.Iterated        (Iterated)
import           Subclasses.MultiComposed.IteratedTH      ()
import           Subclasses.MultiComposed.MultiComposedTH ()
import qualified Subclasses.Symmetric                     as Symm
import           Subclasses.Symmetric                     (SymmetricFun)
import qualified Subclasses.Threshold                     as Thresh
import           Subclasses.Threshold                     (NonSymmThresholdFun)
import           Test.QuickCheck                          (Arbitrary (arbitrary, shrink),
                                                           Property,
                                                           Testable (property),
                                                           chooseInt, conjoin,
                                                           elements, sized,
                                                           total, vector, (=/=),
                                                           (===))
import           Test.QuickCheck.Gen                      (Gen)

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
propFlipOutputComplexity gf = complexity gf === complexity (notG gf)

-- The complexity should be mirrored in x=1/2 when inverting the inputs
propFlipInputComplexity :: GenFun -> Property
propFlipInputComplexity gf = propIsMirrorPW (1 % 2)
  (complexity gf)
  (complexity (flipInputsGenFun gf))

------------------- Minimization ------------------------------------

-- After minimizing a GenFun, it should have the variables [1 .. n], where
-- n is the number of variables in the original GenFun.
propMinimizedCorrectVars :: GenFun -> Property
propMinimizedCorrectVars gf = (vars, arity)  === ([1 .. n], n)
  where
    ngf = mkNGF gf
    vars = sort $ variables ngf
    arity = ngfArity ngf
    n = length $ variables gf

-- Minimization should not affect the complexity of a GenFun
propMinimizedComplexity :: GenFun -> Property
propMinimizedComplexity gf = complexity gf === complexity (mkNGF gf)

-------------------- Algorithms ----------------------------------
-- Properties comparing the correctness of the complexity algorithms

-- complexity and genAlg should yield equivalent PWs for the same function
propComputeMinCorrect :: GenFun -> Property
propComputeMinCorrect gf =
  pieces (complexity gf) ===
  pieces (minPWs $ map piecewiseFromPoly $ Set.toList $ genAlgThinMemoPoly gf)

-- complexity and explicitComplexity should yield the same result for the same function
propComputeMin'Correct :: GenFun -> Property
propComputeMin'Correct gf = complexity gf === explicitComplexity gf

propIteratedNoLoop :: Iterated NonSymmThresholdFun -> Property
propIteratedNoLoop = total . complexity

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

data IterInput = IterInput Int Int [Bool]
  deriving (Show)

-- The total number of bits will never be higher than n.
instance Arbitrary IterInput where
  arbitrary :: Gen IterInput
  arbitrary = sized $ \limit -> do
    let limit' = if limit == 0 then 1 else limit
    let bitLevels = [(bits, levels) | bits <- [1 .. limit'], levels <- validLevels bits limit']
    inputs <- traverse (\(bits, levels) -> vector (bits ^ levels)) bitLevels
    elements $ zipWith (\(bits, levels) input -> IterInput bits levels input ) bitLevels inputs

validLevels :: Int -> Int -> [Int]
validLevels bits limit = takeWhile (\levels -> bits ^ levels <= limit) [1 .. limit]

newtype IterMajInput = IMI IterInput
  deriving (Show)

instance Arbitrary IterMajInput where
  arbitrary :: Gen IterMajInput
  arbitrary = do
    IterInput bits levels input <- arbitrary
    let (bits', input') = if even bits then (bits - 1, take (levels ^ bits') input) else (bits, input)
    return $ IMI $ IterInput bits' levels input'

propIterRepsCorrect :: IterMajInput -> Property
propIterRepsCorrect (IMI (IterInput bits levels input)) = conjoin [
    resSymm === resGen,
    resThresh === resGen
  ]
  where
    majGeneral = Gen.iteratedMajFun bits levels
    majSymm = Symm.iteratedMajFun bits levels
    majThreshold = Thresh.iteratedMajFun bits levels
    resGen = evalNonSymmetric majGeneral input
    resSymm = evalNonSymmetric majSymm input
    resThresh = evalNonSymmetric majThreshold input

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
    gen = complexity $ Gen.iteratedMajFun bits levels
    symm = complexity $ Symm.iteratedMajFun bits levels
    thresh = complexity $ Thresh.iteratedMajFun bits levels-}

------------------ Algebraic numbers --------------------------------

-- The sign of a polynomial at a rational point should not depend on whether r is
-- expressed as a rational number or as an algebraic number.
propRationalSign :: Rational -> Poly Rational -> Property
propRationalSign r p = s1 === s2
  where
    x = toAlgebraic r
    s1 = signAtRational r p
    s2 = signAtAlgebraic x p

propAlgebraicTranslation :: Algebraic -> Rational -> Property
propAlgebraicTranslation a r = case compare r 0 of
  EQ -> res === EQ
  LT -> res === LT
  GT -> res === GT
  where
    a' = translateRational a r
    res = compare a' a

----------------- Critical points ------------------------------

data CritLimitInput = CLI Algebraic (Poly Rational) Algebraic
  deriving (Show)

instance Arbitrary CritLimitInput where
  arbitrary :: Gen CritLimitInput
  arbitrary = do
    p <- genNonZeroPoly
    a <- arbitrary
    b <- arbitrary
    let low = min a b
    let high = max a b
    let high' = if low == high then translateRational high 1 else high
    return $ CLI low p high'

propMaxNumCritical :: CritLimitInput -> Property
propMaxNumCritical (CLI low p high) = property $ length criticals < degree p
  where
    criticals = criticalPointsInPiece low p high

propCriticalSwitches :: CritLimitInput -> Property
propCriticalSwitches (CLI low p high) = property $ correctSwitches $ map snd criticals
  where
    criticals = determineUncertain $ criticalPointsInPiece low p high

correctSwitches :: [Critical] -> Bool
correctSwitches [] = True
correctSwitches (extreme : xs) = case xs of
  []           -> True
  (next : xs') -> (extreme /= next) && correctSwitches (next : xs')

newtype CritInput = CritInput (Poly Rational)
  deriving (Show)

instance Arbitrary CritInput where
  arbitrary :: Gen CritInput
  arbitrary = do
    (P factors) <- genNonZeroPoly
    d <- chooseInt (1, 3)
    let factors' = take (d + 1) factors
    let p' = P factors'
    if degree p' <= 0
      then arbitrary
      else return $ CritInput p'

possibleCrits :: Int -> [[Critical]]
possibleCrits 1 = [[]]
possibleCrits 2 = [[Minimum], [Maximum]]
possibleCrits 3 = [[], [Minimum, Maximum], [Maximum, Minimum]]
possibleCrits _ = error "Not yet implemented"

propKnownCrits :: CritInput -> Property
propKnownCrits (CritInput p) = property $ any (crits `isInfixOf`) possible
  where
    deg = degree p
    possible = possibleCrits deg
    crits = map snd $ determineUncertain $ criticalPointsInPiece (Rational (-999999999)) p (Rational 999999999)
