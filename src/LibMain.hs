module LibMain (main) where
import           Algorithm.GenAlg   (genAllBoths)
import           DSLsofMath.Algebra (AddGroup, MulGroup)
import           Filters            (degreePred, maximaPred)
import           Poly.PiecewisePoly (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils         (minDegree)

main :: IO ()
main = mapM_ print (findSimplest :: [BothPW Rational])

findSimplest :: (AddGroup a, MulGroup a, Real a, Show a) => [BothPW a]
findSimplest = filter (toBoth (degreePred (== minDegree'))) allWith2maxima
  where
    minDegree' = minDegree $ map (\(BothPW pw _) -> pw) allWith2maxima
    allWith2maxima = filter (toBoth (maximaPred (== 2))) $ genAllBoths 4


toBoth :: (PiecewisePoly a -> b) -> (BothPW a -> b)
toBoth f (BothPW pw _) = f pw
