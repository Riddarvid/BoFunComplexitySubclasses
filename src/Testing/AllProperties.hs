{-# LANGUAGE ExistentialQuantification #-}
module Testing.AllProperties (testAll) where

import           Test.QuickCheck    (Args (maxSize, maxSuccess), Testable,
                                     quickCheckWith, stdArgs)
import           Testing.Properties (propComplexityNot, propComputeMin'Correct,
                                     propConversionIteratedThreshold,
                                     propConversionSymm, propCorrectComplexity,
                                     propFlipAllInputs, propFlipCorrect,
                                     propFlipOutput, propMajEqual,
                                     propNormalizedComplexity,
                                     propNormalizedCorrectVars,
                                     propRationalSign, propSameComplexity)

data Check = forall p. Testable p => Check p Args

stdArgs' :: Args
stdArgs' = stdArgs{maxSuccess = 200}

allProps :: [Check]
allProps = [
  Check propRationalSign stdArgs'{maxSize = 5},
  Check propComplexityNot stdArgs'{maxSize = 5},
  Check propNormalizedCorrectVars stdArgs'{maxSize = 10},
  Check propNormalizedComplexity stdArgs'{maxSize = 5},
  Check propMajEqual stdArgs'{maxSize = 5},
  Check propSameComplexity stdArgs',
  Check propFlipCorrect stdArgs'{maxSize = 10},
  Check propFlipOutput stdArgs'{maxSize = 5},
  Check propFlipAllInputs stdArgs'{maxSize = 5},
  Check propCorrectComplexity stdArgs'{maxSize = 5},
  Check propConversionSymm stdArgs'{maxSize = 10},
  Check propConversionIteratedThreshold stdArgs'{maxSize = 7},
  Check propComputeMin'Correct stdArgs'{maxSize = 5, maxSuccess = 1000}
  ]

testAll :: IO ()
testAll = mapM_ (\(n, Check p args) -> print n >> quickCheckWith args p)
  $ zip [1 :: Int ..] allProps