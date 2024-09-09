{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PolyInstances () where
import           Algorithm.Algor    (Algor (..))
import           DSLsofMath.Algebra (Additive (zero, (+)),
                                     Multiplicative (one, (*)), Ring, (-))
import           DSLsofMath.PSDS    (Poly, xP)
import           PolyCmp            (OrdField, cmpPoly)
import           Prelude            hiding ((*), (+), (-))
import           Thin               (Thin (cmp))


instance OrdField a => Thin (Poly a) where
  cmp = cmpPoly

instance Ring a => Algor (Poly a) Int where
  res = resPoly
  pic = pickPoly

resPoly :: Ring a => Bool -> a
resPoly _b = zero
pickPoly :: Ring a => i -> Poly a -> Poly a -> Poly a
pickPoly _i = pickPoly'

pickPoly' :: Ring a => Poly a -> Poly a -> Poly a
pickPoly' p0 p1 = one + (one - xP)*p0 + xP*p1
