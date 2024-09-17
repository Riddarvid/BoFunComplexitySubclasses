module LibMain (main, waysToChooseSubFunctions) where
import           Algebraic          (Algebraic (Algebraic), toAlgebraic)
import           Algorithm.GenAlg   (genAllBoths)
import           Data.Ratio         ((%))
import           DSLsofMath.Algebra (AddGroup, Additive ((+)), MulGroup, (*))
import           DSLsofMath.PSDS    (Poly (P))
import           Filters            (degreePred, maximaPred)
import           Poly.PiecewisePoly (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils         (countMaxima, minDegree)
import           Prelude            hiding ((*), (+))

main :: IO ()
main = print test --mapM_ (print . (\(BothPW pw _) -> countMaxima pw)) (genAllBoths 3 :: [BothPW Rational])
  -- print $ countMaxima $ (\(BothPW pw _) -> pw) ((genAllBoths 4 :: [BothPW Rational]) !! 6)

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

test :: Bool
test = a + b == b + a
  where
    -- a represents the number 1
    a = Algebraic (P [(-1) % 1,0 % 1,1 % 1]) ((-91) % 1,4 % 5)
    -- b represents the number -sqrt(8)
    b = Algebraic (P [0 % 1,(-1) % 1,1 % 1]) (1 % 10,91 % 1)
