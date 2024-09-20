{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main,
  waysToChooseSubFunctions
) where
import           Algorithm.GenAlg         (genAllBoths)
import           BDD                      (BDDFun)
import           Data.DecisionDiagram.BDD (AscOrder)
import           DSLsofMath.Algebra       (AddGroup, MulGroup, (*))
import           Filters                  (degreePred, maximaPred)
import           Poly.PiecewisePoly       (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils               (minDegree)
import           Prelude                  hiding ((*), (+))
import           PrettyPrinting           (desmosShowPW)
import           Subclasses.Comparisons   (benchBoFun, complexityBench)
import           Subclasses.GeneralBDD    (majBDD)
import           Subclasses.IdConst       ()
import           Subclasses.Symmetric     (symmMaj, symmMajBasic)
import           Threshold                (ThresholdFun, thresholdFunReplicate,
                                           thresholdMaj)

-- TODO lägg in i ett set för att få unika
main :: IO ()
main = do
  let with2 = filter (maximaPred (>= 2)) $ map (\(BothPW pw _) -> pw) (genAllBoths 4 :: [BothPW Rational])
  mapM_ (putStrLn . desmosShowPW) with2

mainBench :: IO ()
mainBench = benchBoFun "maj9"
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

-- We want to use this to count the number of threshold functions
waysToChooseSubFunctions :: Integer -> Integer
waysToChooseSubFunctions 0 = 0
waysToChooseSubFunctions 1 = 4
waysToChooseSubFunctions n = sum $ map (\i -> 2^(2^i) * waysToChooseSubFunctions (n - i)) [1 .. n]
