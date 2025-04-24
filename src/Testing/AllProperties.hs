{-# LANGUAGE ExistentialQuantification #-}
module Testing.AllProperties (testAll) where

import           Test.QuickCheck    (Args (maxSize, maxSuccess), Testable,
                                     quickCheckWith, stdArgs)
import           Testing.Properties (propAlgebraicTranslation,
                                     propConversionSymm, propCriticalSwitches,
                                     propExplicitpiecewiseComplexityCorrect,
                                     propFlipInputComplexity,
                                     propFlipOutputComplexity,
                                     propFlipOutputCorrect, propIterRepsCorrect,
                                     propIteratedNoLoop, propKnownCrits,
                                     propMaxNumCritical,
                                     propMinimizedComplexity,
                                     propMinimizedCorrectVars,
                                     propPiecewiseComplexityCorrect,
                                     propRationalSign, propRepsCorrect)

data Check = forall p. Testable p => Check p Args

stdArgs' :: Args
stdArgs' = stdArgs{maxSuccess = 200}

allProps :: [Check]
allProps = [
  Check propIteratedNoLoop stdArgs'{maxSize = 10},
  Check propRationalSign stdArgs'{maxSize = 5},
  Check propAlgebraicTranslation stdArgs'{maxSize = 7},
  Check propMinimizedCorrectVars stdArgs'{maxSize = 10},
  Check propMinimizedComplexity stdArgs'{maxSize = 5},
  Check propRepsCorrect stdArgs'{maxSize = 5},
  Check propIterRepsCorrect stdArgs'{maxSize = 5},
  Check propFlipOutputCorrect stdArgs'{maxSize = 10},
  Check propFlipOutputComplexity stdArgs'{maxSize = 5},
  Check propFlipInputComplexity stdArgs'{maxSize = 5},
  Check propConversionSymm stdArgs'{maxSize = 10},
  Check propPiecewiseComplexityCorrect stdArgs'{maxSize = 5},
  Check propExplicitpiecewiseComplexityCorrect stdArgs'{maxSize = 5},
  Check propMaxNumCritical stdArgs'{maxSize = 6},
  Check propCriticalSwitches stdArgs'{maxSize = 6},
  Check propKnownCrits stdArgs'
  ]

testAll :: IO ()
testAll = mapM_ (\(n, Check p args) -> print n >> quickCheckWith args p)
  $ zip [1 :: Int ..] allProps
