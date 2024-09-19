{-# LANGUAGE FlexibleContexts #-}
module LibMain (main, waysToChooseSubFunctions) where
import           Algebraic                (Algebraic (Algebraic),
                                           MyMatrix (MyMatrix), testMtoPoly,
                                           toCharacteristicPoly)
import           Algorithm.GenAlg         (genAllBoths)
import           BDD                      (BDDFun)
import           Data.DecisionDiagram.BDD (AscOrder)
import           Data.Maybe               (fromJust)
import           Data.Ratio               ((%))
import           DSLsofMath.Algebra       (AddGroup, Additive ((+)), MulGroup,
                                           (*))
import           DSLsofMath.PSDS          (Poly (P), divModP)
import           Filters                  (degreePred, maximaPred)
import           MatrixBridge             (Matrix, fromLists, getDiag)
import           Poly.PiecewisePoly       (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils               (minDegree)
import           Prelude                  hiding ((*), (+))
import           Subclasses.Comparisons   (benchBoFun, complexityBench)
import           Subclasses.GeneralBDD    (majBDD)
import           Subclasses.IdConst       ()
import           Subclasses.Symmetric     (symmMaj, symmMajBasic)
import           Threshold                (ThresholdFun, thresholdFunReplicate,
                                           thresholdMaj)

--main :: IO ()
--main = print $ divModP (P [-1 :: Rational, 4, 1]) (P [-2, 0, 1])

genMatrix :: Int -> Matrix Int
genMatrix n = fromLists $ map (\n' -> [n' .. n + n' - 1]) [1 .. n]

main :: IO ()
main = benchBoFun "maj9"
  [
    complexityBench "symmetric basic maj9" (symmMajBasic 9),
    complexityBench "symmetric maj9" (symmMaj 9),
    complexityBench "threshold maj9" (thresholdFunReplicate (thresholdMaj 9) Nothing :: ThresholdFun (Maybe Bool)),
    complexityBench "generic maj9" (majBDD 9 :: BDDFun AscOrder)
  ]

findSimplest :: (AddGroup a, MulGroup a, Real a, Show a) => [BothPW a]
findSimplest = filter (toBoth (degreePred (== minDegree'))) allWith2maxima
  where
    minDegree' = minDegree $ map (\(BothPW pw _) -> pw) allWith2maxima
    allWith2maxima = filter (toBoth (maximaPred (== 2))) $ genAllBoths 4


toBoth :: (PiecewisePoly a -> b) -> (BothPW a -> b)
toBoth f (BothPW pw _) = f pw

waysToChooseSubFunctions :: Integer -> Integer
waysToChooseSubFunctions 0 = 0
waysToChooseSubFunctions 1 = 4
waysToChooseSubFunctions n = sum $ map (\i -> 2^(2^i) * waysToChooseSubFunctions (n - i)) [1 .. n]
{-}
test :: Bool
test = a + b == b + a
  where
    -- a represents the number 1
    a = Algebraic (P [(-1) % 1,0 % 1,1 % 1]) ((-91) % 1,4 % 5)
    -- b represents the number -sqrt(8)
    b = Algebraic (P [0 % 1,(-1) % 1,1 % 1]) (1 % 10,91 % 1)
-}
