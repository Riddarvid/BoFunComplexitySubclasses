{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs          #-}
module Poly.PolyInstances () where
import           Algorithm.Algor    (Algor (..))
import           DSLsofMath.Algebra (Additive (zero, (+)),
                                     Multiplicative (one, (*)), Ring, (-))
import           DSLsofMath.PSDS    (Poly, xP)
import           Poly.PolyCmp       (OrdField, cmpPoly)
import           Prelude            hiding ((*), (+), (-))
import           Thin               (Thin (cmp))


instance OrdField a => Thin (Poly a) where
  cmp :: Poly a -> Poly a -> Maybe Ordering
  cmp = cmpPoly

instance Ring a => Algor (Poly a) where
  res :: Bool -> Poly a
  res = resPoly
  pic :: Int -> Poly a -> Poly a -> Poly a
  pic = pickPoly

resPoly :: Ring a => Bool -> a
resPoly _b = zero
pickPoly :: Ring a => i -> Poly a -> Poly a -> Poly a
pickPoly _i = pickPoly'

pickPoly' :: Ring a => Poly a -> Poly a -> Poly a
pickPoly' p0 p1 = one + (one - xP)*p0 + xP*p1
