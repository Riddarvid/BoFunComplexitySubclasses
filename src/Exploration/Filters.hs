{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Exploration.Filters (
  piecesPred,
  degreePred,
  criticalPred
) where
import           Control.Arrow        ((>>>))
import           Debug.Trace          (traceShowId)
import           DSLsofMath.Algebra   (AddGroup, MulGroup)
import           Exploration.Critical (CriticalPoint, findCritcalPointsPW)
import           Poly.PiecewisePoly   (PiecewisePoly)
import           Poly.PolyInstances   ()
import           Poly.Utils           (countPieces, findDegreePW)
import           Prelude              hiding (negate, (+), (/))

piecesPred :: (AddGroup a, MulGroup a, Eq a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
piecesPred p = countPieces >>> p

degreePred :: (Eq a, AddGroup a, MulGroup a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
degreePred p = findDegreePW >>> p

criticalPred :: ([CriticalPoint] -> Bool) -> (PiecewisePoly Rational -> Bool)
criticalPred p = findCritcalPointsPW >>> p
