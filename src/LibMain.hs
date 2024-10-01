{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main
) where
import           Algorithm.GenAlg         (genAllBoths)
import           Algorithm.GenAlgPW       (computeMin)
import           BDD                      (BDDFun, flipInputs)
import           Control.DeepSeq          (force)
import           Control.Exception        (evaluate)
import           Control.Monad            (void)
import           Data.DecisionDiagram.BDD (notB, var, (.&&.), (.||.))
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
import           Subclasses.Comparisons   (mainBench, mainBenchMaj)
import qualified Subclasses.General       as Gen
import           Subclasses.Id            ()
import           Test.QuickCheck          (Arbitrary (arbitrary), generate)

main :: IO ()
main = print $ mirrorP (1 % 2) (P [1 :: Rational, 1])

main9 :: IO ()
main9 = do
  bf <- generate arbitrary :: IO BDDFun
  let bf' = flipInputs bf
  desmosPrintPW $ computeMin bf
  desmosPrintPW $ computeMin bf'

tOR :: BDDFun
tOR = var 0 .||. var 1

tNAND :: BDDFun
tNAND = notB (var 0 .&&. var 1)

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
