{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main
) where
import           Algorithm.GenAlg            (genAlgThinMemo, piecewiseBoth)
import           Algorithm.GenAlgPW          (computeMin)
import           BDD                         (BDDFun, normalizeBDD)
import           Control.DeepSeq             (force)
import           Control.Exception           (evaluate)
import           Control.Monad               (void)
import           Data.DecisionDiagram.BDD    (AscOrder, BDD, notB, var, (.&&.),
                                              (.||.))

import qualified Data.Set                    as Set
import           DSLsofMath.Algebra          (AddGroup, MulGroup)
import           Filters                     (degreePred, maximaPred)
import           Poly.PiecewisePoly          (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils                  (minDegree)
import           Prelude                     hiding ((*), (+))
import           PrettyPrinting              (desmosPrintPW, desmosShowPW)
import           Subclasses.Comparisons      (mainBenchMaj,
                                              measureComplexityTime,
                                              measureComplexityTime',
                                              measureComplexityTime'')
import qualified Subclasses.General          as Gen
import           Subclasses.General          (GenFun (GenFun), allGenFuns,
                                              flipInputsGenFun)
import           Subclasses.Id               ()
import           Subclasses.NormalizedGenFun (mkNGF)
import qualified Subclasses.Symmetric        as Symm
import qualified Subclasses.Threshold        as Thresh
import           Test.QuickCheck             (Arbitrary (arbitrary), generate)

main :: IO ()
main = main10

main11 :: IO ()
main11 = print (and12 == and23, and12 == and23')
  where
    and12 = var 1 .&&. var 2 :: BDD AscOrder
    and23 = var 3 .&&. var 2 :: BDD AscOrder
    (and23', _) = normalizeBDD and23

main10 :: IO ()
main10 = measureComplexityTime' (mkNGF $ Gen.iteratedMajFun 3 2) >>= print

main9 :: IO ()
main9 = do
  gf <- generate arbitrary :: IO GenFun
  let gf' = flipInputsGenFun gf
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

