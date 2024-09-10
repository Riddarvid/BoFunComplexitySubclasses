{-# LANGUAGE FlexibleContexts #-}
module Algorithm.GenAlg (
  genAlg,
  genAlgMemo,
  genAlgThinMemo,
  genAlgThinMemoPoly
) where
import           Algorithm.Algor       (Algor (pic), res)
import           BDD.BDDInstances      ()
import           BoFun                 (BoFun (isConst, setBit, variables))
import           Data.Function.Memoize (Memoizable, memoFix)
import qualified Data.Set              as S
import           DSLsofMath.Algebra    ()
import           DSLsofMath.PSDS       (Poly)
import           PolyInstances         ()
import           Thin                  (Thin (thin))

-- Naive
genAlg :: (BoFun fun i, Algor a, Ord a) => fun -> S.Set a
genAlg = genAlgStep genAlg

-- Add memoization
genAlgMemo :: (Memoizable fun, BoFun fun Int) => fun -> S.Set (Poly Int)
genAlgMemo = memoFix genAlgStep

-- Add thinning and memoization
genAlgThinMemo :: (BoFun fun i, Memoizable fun, Thin a, Algor a) => fun -> S.Set a
genAlgThinMemo = memoFix genAlgStepThin

-- Same as above but type specified to Poly Rational
genAlgThinMemoPoly :: (Memoizable fun, BoFun fun Int) => fun -> S.Set (Poly Rational)
genAlgThinMemoPoly = genAlgThinMemo

-- Same as above but type specified to (Poly Rational, DecTree)
--genAlgThinMemoPolyAndTree :: BDDFun a -> S.Set (Poly Rational, DecTree)
--genAlgThinMemoPolyAndTree = genAlgThinMemo

-- Alg steps

genAlgStep ::  (BoFun fun i, Algor a, Ord a) =>
               (fun -> S.Set a) ->
               (fun -> S.Set a)
genAlgStep _ fun
  | Just b <- isConst fun = S.singleton (res b)
genAlgStep genAlg' fun =
  S.fromList $ do
    (n, i) <- zip [1 ..] $ variables fun
    p0 <- S.toList (genAlg' (setBit (i, False) fun))
    p1 <- S.toList (genAlg' (setBit (i, True) fun))
    return $ pic n p0 p1

genAlgStepThin ::
  (Thin a, BoFun fun i, Algor a) =>
  (fun -> S.Set a) ->
  (fun -> S.Set a)
genAlgStepThin genAlg' f = thin (genAlgStep genAlg' f)
