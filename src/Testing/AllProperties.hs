{-# LANGUAGE ExistentialQuantification #-}
module Testing.AllProperties (testAll) where

import           Test.QuickCheck    (Args (maxSize, maxSuccess), Testable,
                                     quickCheckWith, stdArgs)
import           Testing.Properties (propComputeMin'Correct,
                                     propComputeMinCorrect, propConversionSymm,
                                     propFlipInputComplexity,
                                     propFlipOutputComplexity,
                                     propFlipOutputCorrect, propIterRepsCorrect,
                                     propNormalizedComplexity,
                                     propNormalizedCorrectVars,
                                     propRationalSign, propRepsCorrect)

data Check = forall p. Testable p => Check p Args

stdArgs' :: Args
stdArgs' = stdArgs{maxSuccess = 200}

allProps :: [Check]
allProps = [
  Check propRationalSign stdArgs'{maxSize = 5},
  Check propNormalizedCorrectVars stdArgs'{maxSize = 10},
  Check propNormalizedComplexity stdArgs'{maxSize = 5},
  Check propRepsCorrect stdArgs'{maxSize = 5},
  Check propIterRepsCorrect stdArgs'{maxSize = 5},
  Check propFlipOutputCorrect stdArgs'{maxSize = 10},
  Check propFlipOutputComplexity stdArgs'{maxSize = 5},
  Check propFlipInputComplexity stdArgs'{maxSize = 5},
  Check propConversionSymm stdArgs'{maxSize = 10},
  Check propComputeMinCorrect stdArgs'{maxSize = 5},
  Check propComputeMin'Correct stdArgs'{maxSize = 5, maxSuccess = 1000}
  ]

testAll :: IO ()
testAll = mapM_ (\(n, Check p args) -> print n >> quickCheckWith args p)
  $ zip [1 :: Int ..] allProps
