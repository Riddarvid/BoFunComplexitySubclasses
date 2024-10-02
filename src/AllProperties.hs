{-# LANGUAGE ExistentialQuantification #-}
module AllProperties (
  propRationalSign,

  propComplexityNot,
  propNormalizedEqual,
  propNormalizedEval,

  propMajEqual,
  propSameComplexity,
  testAll,

  propFlipCorrect,
  propFlipOutput,
  propFlipAllInputs
) where
import           Filters                (propRationalSign)
import           Properties             (propComplexityNot,
                                         propCorrectComplexity,
                                         propFlipAllInputs, propFlipCorrect,
                                         propFlipOutput, propNormalizedEqual,
                                         propNormalizedEval)
import           Subclasses.Comparisons (propMajEqual, propSameComplexity)
import           Test.QuickCheck        (Args (maxSize), Testable,
                                         quickCheckWith, stdArgs)

data Check = forall p. Testable p => Check p Args

allProps :: [Check]
allProps = [
  Check propRationalSign stdArgs{maxSize = 5},
  Check propComplexityNot stdArgs{maxSize = 5},
  Check propNormalizedEqual stdArgs,
  Check propNormalizedEval stdArgs,
  Check propMajEqual stdArgs{maxSize = 5},
  Check propSameComplexity stdArgs,
  Check propFlipCorrect stdArgs,
  Check propFlipOutput stdArgs{maxSize = 5},
  Check propFlipAllInputs stdArgs{maxSize = 5},
  Check propCorrectComplexity stdArgs{maxSize = 5}
  ]

testAll :: IO ()
testAll = mapM_ (\(n, Check p args) -> print n >> quickCheckWith args p)
  $ zip [1 :: Int ..] allProps
