{-# LANGUAGE ExistentialQuantification #-}
module AllProperties (
  propRationalSign,

  propComplexityNot,
  propNormalizedCorrectVars,

  propMajEqual,
  propSameComplexity,
  testAll,

  propFlipCorrect,
  propFlipOutput,
  propFlipAllInputs
) where
import           Filters                (propRationalSign)
import           Properties             (propComplexityNot,
                                         propConversionIteratedThreshold,
                                         propConversionSymm,
                                         propCorrectComplexity,
                                         propFlipAllInputs, propFlipCorrect,
                                         propFlipOutput,
                                         propNormalizedComplexity,
                                         propNormalizedCorrectVars)
import           Subclasses.Comparisons (propMajEqual, propSameComplexity)
import           Test.QuickCheck        (Args (maxSize), Testable,
                                         quickCheckWith, stdArgs)

data Check = forall p. Testable p => Check p Args

allProps :: [Check]
allProps = [
  Check propRationalSign stdArgs{maxSize = 5},
  Check propComplexityNot stdArgs{maxSize = 5},
  Check propNormalizedCorrectVars stdArgs{maxSize = 10},
  Check propNormalizedComplexity stdArgs{maxSize = 5},
  Check propMajEqual stdArgs{maxSize = 5},
  Check propSameComplexity stdArgs,
  Check propFlipCorrect stdArgs{maxSize = 10},
  Check propFlipOutput stdArgs{maxSize = 5},
  Check propFlipAllInputs stdArgs{maxSize = 5},
  Check propCorrectComplexity stdArgs{maxSize = 5},
  Check propConversionSymm stdArgs{maxSize = 10},
  Check propConversionIteratedThreshold stdArgs{maxSize = 7}
  ]

testAll :: IO ()
testAll = mapM_ (\(n, Check p args) -> print n >> quickCheckWith args p)
  $ zip [1 :: Int ..] allProps
