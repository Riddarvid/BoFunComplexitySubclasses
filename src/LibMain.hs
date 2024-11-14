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

import qualified Data.HashSet                         as HS
import qualified Data.Set                             as Set
import           Data.Time                            (NominalDiffTime)
import           DSLsofMath.Algebra                   (AddGroup, MulGroup)
import           Exploration.Comparisons              (mainBenchMaj,
                                                       measureTimeComputeMin,
                                                       measureTimeComputeMin',
                                                       measureTimeGenAlg)
import           Exploration.Filters                  (degreePred, maximaPred)
import           Poly.PiecewisePoly                   (BothPW (BothPW),
                                                       PiecewisePoly)
import           Poly.Utils                           (minDegree)
import           Prelude                              hiding ((*), (+))
import           Subclasses.CanonicalGenFun           (CanonicalGenFun, mkCGF)
import qualified Subclasses.GenFun                    as Gen
import           Subclasses.GenFun                    (GenFun (GenFun),
                                                       allGenFuns,
                                                       flipInputsGenFun)
import           Subclasses.Id                        ()
import           Subclasses.Iterated                  (IteratedSymm)
import           Subclasses.IteratedInstances         ()
import           Subclasses.NormalizedCanonicalGenFun (NormalizedCanonicalGenFun,
                                                       mkNCGF)
import           Subclasses.NormalizedGenFun          (NormalizedGenFun, mkNGF)
import qualified Subclasses.Symmetric                 as Symm
import           Subclasses.Symmetric                 (SymmetricFun)
import qualified Subclasses.Threshold                 as Thresh
import           Subclasses.Threshold                 (ThresholdFun)
import           Test.QuickCheck                      (Arbitrary (arbitrary),
                                                       generate)
import           Testing.PrettyPrinting               (desmosPrintPW,
                                                       desmosShowPW)
import           Timing                               (measureMajs,
                                                       measureRandomFuns)

main :: IO ()
main = do
  t <- measureTimeComputeMin $ Gen.majFun 11
  print t
  --vals <- measureRandomFuns (measureTimeComputeMin :: IteratedSymm SymmetricFun -> IO NominalDiffTime) 30 3 10
  --print vals

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
  let with2 = filter (maximaPred (>= 2)) $ map (\(BothPW pw _) -> pw) (genAllBoths 4 :: [BothPW Rational])
  let unique = Set.toList $ Set.fromList with2
  mapM_ (putStrLn . desmosShowPW) unique
  print $ length unique

findSimplest :: (AddGroup a, MulGroup a, Real a, Show a) => [BothPW a]
findSimplest = filter (toBoth (degreePred (== minDegree'))) allWith2maxima
  where
    minDegree' = minDegree $ map (\(BothPW pw _) -> pw) allWith2maxima
    allWith2maxima = filter (toBoth (maximaPred (== 2))) $ genAllBoths 4

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

