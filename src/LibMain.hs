{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main
) where
import           BDD.BDD                                     (BDDa,
                                                              normalizeBDD)
import           Control.DeepSeq                             (NFData, force)
import           Control.Exception                           (evaluate)
import           Control.Monad                               (void)
import           Data.DecisionDiagram.BDD                    (AscOrder, BDD,
                                                              notB, var, (.&&.),
                                                              (.||.))

import           Algebraic                                   (AlgRep (AlgRep),
                                                              Algebraic (Algebraic, Rational),
                                                              translateRational)
import           Arity                                       (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun                            (BoFun)
import           Complexity.GenAlg                           (genAlgThinMemoPoly)
import           Complexity.Piecewise                        (complexity,
                                                              complexityAndAlgorithms)
import           Data.Function.Memoize                       (Memoizable)
import           Data.List.NonEmpty                          (NonEmpty ((:|)))
import           Data.Ratio                                  ((%))
import           Data.Time                                   (NominalDiffTime,
                                                              diffUTCTime,
                                                              getCurrentTime)
import           DSLsofMath.Algebra                          (AddGroup,
                                                              MulGroup)
import           DSLsofMath.PSDS                             (Poly (P))
import           Exploration.Critical                        (Critical (Maximum),
                                                              CriticalPoint,
                                                              critcalPointsPW,
                                                              criticalPointsInPiece,
                                                              determineUncertain)
import           Exploration.Filters                         (criticalPred)
import           Exploration.Measurements                    (measureSingleStdOut,
                                                              measureSpecificStdOut,
                                                              measureTimeGenAlg,
                                                              measureTimePiecewiseComplexity,
                                                              measureTimePiecewiseExplicitComplexity)
import           Exploration.PrettyPrinting                  (PrettyBoFun (prettyPrint),
                                                              desmosPrintPW)
import           Poly.PiecewisePoly                          (BothPW (BothPW),
                                                              PiecewisePoly,
                                                              printPW)
import           Poly.PolynomialExtra                        (translateInput)
import           Prelude                                     hiding ((*), (+))
import           Subclasses.Gates                            (iterAnd, iterOr)
import           Subclasses.GenFun.CanonicalGenFun           (CanonicalGenFun)
import qualified Subclasses.GenFun.GenFun                    as Gen
import           Subclasses.GenFun.GenFun                    (GenFun (GenFun),
                                                              allGenFuns,
                                                              flipInputsGenFun,
                                                              prettyPrintGenFun)
import           Subclasses.GenFun.NormalizedCanonicalGenFun (NormalizedCanonicalGenFun)
import           Subclasses.GenFun.NormalizedGenFun          (NormalizedGenFun)
import           Subclasses.Iterated.Iterated                (Iterated' (Id),
                                                              iterateFun)
import           Subclasses.Iterated.IteratedTH              ()
import           Subclasses.Lifted                           ()
import           Subclasses.Symmetric                        (mkNonSymmSymmetricFun,
                                                              mkSymmetricFun)
import qualified Subclasses.Threshold                        as Thresh
import           Subclasses.Threshold                        (ThresholdFun (ThresholdFun))
import           System.Environment                          (getArgs)
import           Test.QuickCheck                             (Arbitrary (arbitrary),
                                                              generate)

main :: IO ()
main = do
  let f = Thresh.iteratedMajFun 3 2
  measureTimePiecewiseComplexity f >>= print

main11 :: IO ()
main11 = print (and12 == and23, and12 == and23')
  where
    and12 = var 1 .&&. var 2 :: BDD AscOrder
    and23 = var 3 .&&. var 2 :: BDD AscOrder
    (and23', _) = normalizeBDD and23

main12 :: IO ()
main12 = void $ evaluate $ force $ complexity (Thresh.majFun 301)

main10 :: IO ()
main10 = measureTimePiecewiseComplexity (Thresh.majFun 301) >>= print

main9 :: IO ()
main9 = do
  gf <- generate arbitrary :: IO GenFun
  let gf' = flipInputsGenFun gf
  desmosPrintPW $ complexity gf
  desmosPrintPW $ complexity gf'

tOR :: BDDa
tOR = var 1 .||. var 2

tNAND :: BDDa
tNAND = notB (var 1 .&&. var 2)

main5 :: IO ()
main5 = void $ evaluate $ force $ complexity $ Gen.majFun 11

main2 :: IO ()
main2 = do
  let allFuns = allGenFuns 4
  let comps = map (\f -> (f, complexity f)) allFuns
  let maxMaxima = maximum $ map (countMaxima . critcalPointsPW . snd) comps
  putStrLn ("Max maxima: " ++ show maxMaxima)
  let withMaxMaxima = filter (\(_, c) -> criticalPred (nMax maxMaxima) c) comps
  mapM_ (\(f, c) -> print f >> printPW c) withMaxMaxima
  print $ length withMaxMaxima

nMax :: Int -> [CriticalPoint] -> Bool
nMax n points = countMaxima points == n

countMaxima :: [CriticalPoint] -> Int
countMaxima = length . filter (\(_, t) -> t == Maximum)

{-
findSimplest :: (AddGroup a, MulGroup a, Real a, Show a) => [BothPW a]
findSimplest = filter (toBoth (degreePred (== minDegree'))) allWith2maxima
  where
    minDegree' = minDegree $ map (\(BothPW pw _) -> pw) allWith2maxima
    allWith2maxima = filter (toBoth (criticalPred (== 2))) $ genAllBoths 4-}

toBoth :: (PiecewisePoly a -> b) -> (BothPW a -> b)
toBoth f (BothPW pw _) = f pw

genAllBoths :: (Show a, AddGroup a, MulGroup a, Ord a) => Int -> [BothPW a]
genAllBoths n = map complexityAndAlgorithms (allGenFuns n :: [GenFun])

--------------- test ------------------------

testGF :: GenFun
testGF = GenFun (notB (var 2)) 2

