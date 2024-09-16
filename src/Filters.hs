{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Filters (
  piecesPred,
  degreePred,
  maximaPred
) where
import           BDD.BDDInstances   ()
import           Control.Arrow      ((>>>))
import           DSLsofMath.Algebra (AddGroup, MulGroup)
import           Poly.PiecewisePoly (PiecewisePoly)
import           Poly.Utils         (countMaxima, countPieces, findDegreePW)

piecesPred :: (AddGroup a, MulGroup a, Eq a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
piecesPred p = countPieces >>> p

degreePred :: (Eq a, AddGroup a, MulGroup a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
degreePred p = findDegreePW >>> p

maximaPred :: (Real a, AddGroup a, MulGroup a, Show a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
maximaPred p = countMaxima >>> p
