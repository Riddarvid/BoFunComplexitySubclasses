{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main
) where
import           Algorithm.GenAlg               (piecewiseBoth)
import           Algorithm.GenAlgPW             (computeMin)
import           BDD.BDD                        (BDDa, normalizeBDD)
import           Control.DeepSeq                (force)
import           Control.Exception              (evaluate)
import           Control.Monad                  (void)
import           Data.DecisionDiagram.BDD       (AscOrder, BDD, notB, var,
                                                 (.&&.), (.||.))

import           DSLsofMath.Algebra             (AddGroup, MulGroup)
import           Exploration.Comparisons        (mainBenchMaj,
                                                 measureTimeComputeMin)
import           Exploration.Critical           (Critical (Maximum),
                                                 CriticalPoint)
import           Exploration.Filters            (criticalPred)
import           Poly.PiecewisePoly             (BothPW (BothPW), PiecewisePoly,
                                                 printPW)
import           Prelude                        hiding ((*), (+))
import qualified Subclasses.GenFun.GenFun       as Gen
import           Subclasses.GenFun.GenFun       (GenFun (GenFun), allGenFuns,
                                                 flipInputsGenFun)
import           Subclasses.Iterated.IteratedTH ()
import           Subclasses.Lifted              ()
import qualified Subclasses.Threshold           as Thresh
import           Test.QuickCheck                (Arbitrary (arbitrary),
                                                 generate)
import           Testing.PrettyPrinting         (PrettyBoFun (prettyPrint),
                                                 desmosPrintPW)

main :: IO ()
main = prettyPrint $ Thresh.iteratedMajFun 3 3


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
  let allFuns = allGenFuns 4
  let comps = map (\f -> (f, computeMin f)) allFuns
  let with2maxima = filter (\(_, c) -> criticalPred (nMax 2) c) comps
  mapM_ (\(f, c) -> print f >> printPW c) with2maxima
  print $ length with2maxima

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

genAllBoths :: (Show a, AddGroup a, MulGroup a, Ord a) => Int -> [BothPW a]
genAllBoths n = map piecewiseBoth (allGenFuns n :: [GenFun])

--------------- test ------------------------

testGF :: GenFun
testGF = GenFun (notB (var 2)) 2

