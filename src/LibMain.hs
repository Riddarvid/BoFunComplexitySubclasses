{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibMain (
  main
) where
import           Algorithm.GenAlg     (genAllBoths)
import           Algorithm.GenAlgPW   (computeMin)
import           Control.DeepSeq      (force)
import           Control.Exception    (evaluate)
import qualified Data.Set             as Set
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import           DSLsofMath.Algebra   (AddGroup, MulGroup)
import           Filters              (degreePred, maximaPred)
import           Poly.PiecewisePoly   (BothPW (BothPW), PiecewisePoly)
import           Poly.Utils           (minDegree)
import           Prelude              hiding ((*), (+))
import           PrettyPrinting       (desmosShowPW)
import           Subclasses.Id        ()
import           Subclasses.Symmetric (maj33, majSymm)
import           Subclasses.Threshold (majThreshold)

main :: IO ()
main = putStrLn $ desmosShowPW $ computeMin maj33

main3 :: IO ()
main3 = do
  --print $ computeMin $ majThreshold 301
  --print $ computeMin $ majSymm 301
  -- mainBench 51
  s1 <- getCurrentTime
  _ <- evaluate $ force $ computeMin $ majThreshold 101
  s2 <- getCurrentTime
  _ <- evaluate $ force $ computeMin $ majSymm 101
  s3 <- getCurrentTime
  print (diffUTCTime s2 s1, diffUTCTime s3 s2)

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
