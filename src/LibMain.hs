{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main,
  waysToChooseSubFunctions
) where
import           Algorithm.GenAlg       (genAllBoths)
import qualified Data.Set               as Set
import           DSLsofMath.Algebra     (AddGroup, MulGroup, (*))
import           Filters                (degreePred, maximaPred)
import           Poly.PiecewisePoly     (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils             (minDegree)
import           Prelude                hiding ((*), (+))
import           PrettyPrinting         (desmosShowPW)
import           Subclasses.Comparisons (mainBench)
import           Subclasses.Id          ()

main :: IO ()
main = mainBench

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

-- We want to use this to count the number of threshold functions
waysToChooseSubFunctions :: Integer -> Integer
waysToChooseSubFunctions 0 = 0
waysToChooseSubFunctions 1 = 4
waysToChooseSubFunctions n = sum $ map (\i -> 2^(2^i) * waysToChooseSubFunctions (n - i)) [1 .. n]
