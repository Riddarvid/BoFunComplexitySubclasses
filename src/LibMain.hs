{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main
) where
import           Algorithm.GenAlg                     (genAlgThinMemo,
                                                       genAlgThinMemoPoly,
                                                       piecewiseBoth)
import           Algorithm.GenAlgPW                   (computeMin, computeMin')
import           BDD.BDD                              (BDDa, normalizeBDD)
import           Control.DeepSeq                      (force)
import           Control.Exception                    (evaluate)
import           Control.Monad                        (void)
import           Data.DecisionDiagram.BDD             (AscOrder, BDD, notB, var,
                                                       (.&&.), (.||.))

import           Arity                                (ArbitraryArity (arbitraryArity))
import qualified Data.HashSet                         as HS
import           Data.Ratio                           ((%))
import qualified Data.Set                             as Set
import           Data.Time                            (NominalDiffTime)
import           DSLsofMath.Algebra                   (AddGroup, MulGroup)
import           DSLsofMath.PSDS                      (Poly (P))
import           Exploration.Comparisons              (mainBenchMaj,
                                                       measureTimeComputeMin,
                                                       measureTimeComputeMin',
                                                       measureTimeGenAlg)
import           Exploration.Critical                 (Critical (Maximum),
                                                       CriticalPoint)
import           Exploration.Filters                  (criticalPred, degreePred)
import           Poly.PiecewisePoly                   (BothPW (BothPW),
                                                       PiecewisePoly,
                                                       Separation' (Algebraic))
import           Poly.Utils                           (minDegree,
                                                       numRootsInInterval)
import           Prelude                              hiding ((*), (+))
import           Subclasses.CanonicalGenFun           (CanonicalGenFun, mkCGF)
import qualified Subclasses.GenFun                    as Gen
import           Subclasses.GenFun                    (GenFun (GenFun),
                                                       allGenFuns,
                                                       flipInputsGenFun)
import           Subclasses.Id                        ()
import           Subclasses.Iterated                  (Iterated)
import           Subclasses.IteratedTH                ()
import           Subclasses.Lifted                    (toLifted)
import           Subclasses.NormalizedCanonicalGenFun (NormalizedCanonicalGenFun,
                                                       mkNCGF)
import           Subclasses.NormalizedGenFun          (NormalizedGenFun, mkNGF)
import qualified Subclasses.Symmetric                 as Symm
import           Subclasses.Symmetric                 (SymmetricFun,
                                                       SymmetricFun')
import qualified Subclasses.Threshold                 as Thresh
import           Subclasses.Threshold                 (ThresholdFun,
                                                       ThresholdFun' (ThresholdFun'))
import           Test.QuickCheck                      (Arbitrary (arbitrary),
                                                       generate)
import           Testing.PrettyPrinting               (desmosPrintPW,
                                                       desmosShowPW)
import           Timing                               (measureMajs,
                                                       measureRandomFuns)

main :: IO ()
main = main2


main11 :: IO ()
main11 = print (and12 == and23, and12 == and23')
  where
    and12 = var 1 .&&. var 2 :: BDD AscOrder
    and23 = var 3 .&&. var 2 :: BDD AscOrder
    (and23', _) = normalizeBDD and23

main12 :: IO ()
main12 = void $ evaluate $ force $ computeMin (Thresh.majFun 301)

main10 :: IO ()
main10 = measureTimeComputeMin (Thresh.majFun 301) >>= print

main9 :: IO ()
main9 = do
  gf <- generate arbitrary :: IO GenFun
  let gf' = flipInputsGenFun gf
  desmosPrintPW $ computeMin gf
  desmosPrintPW $ computeMin gf'

tOR :: BDDa
tOR = var 1 .||. var 2

tNAND :: BDDa
tNAND = notB (var 1 .&&. var 2)

main6 :: IO ()
main6 = do
  diffs <- mainBenchMaj 11
  mapM_ print diffs

main5 :: IO ()
main5 = void $ evaluate $ force $ computeMin $ Gen.majFun 11

main2 :: IO ()
main2 = do
  let with2 = filter (criticalPred (nMax 2)) $ map (\(BothPW pw _) -> pw) (genAllBoths 2 :: [BothPW Rational])
  let unique = Set.toList $ Set.fromList with2
  mapM_ (putStrLn . desmosShowPW) unique
  print $ length unique

nMax :: Int -> [CriticalPoint] -> Bool
nMax n points = length (filter (\(_, t) -> t == Maximum) points) == n

{-
findSimplest :: (AddGroup a, MulGroup a, Real a, Show a) => [BothPW a]
findSimplest = filter (toBoth (degreePred (== minDegree'))) allWith2maxima
  where
    minDegree' = minDegree $ map (\(BothPW pw _) -> pw) allWith2maxima
    allWith2maxima = filter (toBoth (criticalPred (== 2))) $ genAllBoths 4-}

toBoth :: (PiecewisePoly a -> b) -> (BothPW a -> b)
toBoth f (BothPW pw _) = f pw

genAllBoths :: (Show a, AddGroup a, MulGroup a, Ord a) =>Int -> [BothPW a]
genAllBoths n = map piecewiseBoth funs
  where
    funs :: [GenFun]
    funs = HS.toList $ allGenFuns n

--------------- test ------------------------

testGF :: GenFun
testGF = GenFun (notB (var 2)) 2

