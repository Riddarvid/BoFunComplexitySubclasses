{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main
) where
import           Algorithm.GenAlg         (piecewiseBoth)
import           Algorithm.GenAlgPW       (computeMin, computeMin')
import           BDD                      (BDDFun)
import           Control.DeepSeq          (force)
import           Control.Exception        (evaluate)
import           Control.Monad            (void)
import           Data.DecisionDiagram.BDD (notB, var, (.&&.), (.||.))

import           Data.Data                (Proxy (Proxy))
import           Data.Ratio               ((%))
import qualified Data.Set                 as Set
import           DSLsofMath.Algebra       (AddGroup, MulGroup)
import           DSLsofMath.PSDS          (Poly (P))
import           Filters                  (degreePred, maximaPred)
import           Poly.PiecewisePoly       (BothPW (BothPW), PiecewisePoly)
import           Poly.PolynomialExtra     (mirrorP)
import           Poly.Utils               (minDegree)
import           Prelude                  hiding ((*), (+))
import           PrettyPrinting           (desmosPrintPW, desmosShowPW)
import           Subclasses.Comparisons   (mainBenchMaj, measureComplexityTime,
                                           measureComplexityTime')
import           Subclasses.Counting      (allIteratedThresholdFuns,
                                           allIteratedThresholdFunsMemo,
                                           averageBDDNodesITF)
import qualified Subclasses.General       as Gen
import           Subclasses.General       (GenFun (GenFun), allGenFuns,
                                           flipInputs, toGenFun)
import           Subclasses.Id            ()
import           Subclasses.Iterated      (Iterated)
import           Subclasses.Symmetric     (BasicSymmetric (BasicSymmetric))
import qualified Subclasses.Threshold     as Thresh
import           Subclasses.Threshold     (ThresholdFun (ThresholdFun))
import           Test.QuickCheck          (Arbitrary (arbitrary), generate)
import           Translations             (genToBasicSymmetricNaive)

main :: IO ()
main = void $ evaluate $ force $ computeMin $ Thresh.iteratedMajFun 3 3

main10 :: IO ()
main10 = measureComplexityTime' (Gen.iteratedMajFun 3 2) >>= print

main9 :: IO ()
main9 = do
  gf <- generate arbitrary :: IO GenFun
  let gf' = flipInputs gf
  desmosPrintPW $ computeMin gf
  desmosPrintPW $ computeMin gf'

tOR :: BDDFun
tOR = var 1 .||. var 2

tNAND :: BDDFun
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
    funs = Set.toList $ allGenFuns n

--------------- test ------------------------

testGF :: GenFun
testGF = GenFun (notB (var 2)) 2

