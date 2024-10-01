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
import           Properties             (propComplexityNot, propFlipAllInputs,
                                         propFlipCorrect, propFlipOutput,
                                         propNormalizedEqual,
                                         propNormalizedEval)
import           Subclasses.Comparisons (propMajEqual, propSameComplexity)
import           Test.QuickCheck        (Testable, quickCheck)

data Check = forall p. Testable p => Check p

allProps :: [Check]
allProps = [
  Check propRationalSign,
  Check propComplexityNot,
  Check propNormalizedEqual,
  Check propNormalizedEval,
  Check propMajEqual,
  Check propSameComplexity,
  Check propFlipCorrect,
  Check propFlipOutput,
  Check propFlipAllInputs
  ]

testAll :: IO ()
testAll = mapM_ (\(Check p) -> quickCheck p) allProps
