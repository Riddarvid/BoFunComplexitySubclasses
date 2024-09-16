module LibMain (main, waysToChooseSubFunctions) where
import           Algebraic          (Algebraic (Algebraic), toAlgebraic)
import           Algorithm.GenAlg   (genAllBoths)
import           DSLsofMath.Algebra (AddGroup, MulGroup, (*))
import           DSLsofMath.PSDS    (Poly (P))
import           Filters            (degreePred, maximaPred)
import           Poly.PiecewisePoly (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils         (minDegree)
import           Prelude            hiding ((*))

main :: IO ()
main = mapM_ print $ filter (toBoth (maximaPred (== 2))) (genAllBoths 4 :: [BothPW Rational])

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

