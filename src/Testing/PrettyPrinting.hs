{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Testing.PrettyPrinting (
  desmosPrintPW,
  desmosShowPW,
  desmosShowP
) where
-- Contains functions for showing polynomials in a way that can be easily copied into
-- the graphical calculator desmos.

import           Data.Ratio         (denominator, numerator)
import           DSLsofMath.Algebra (Additive ((+)), ifThenElse, negate)
import           DSLsofMath.PSDS    (Poly (P))
import           Poly.PiecewisePoly (PiecewisePoly, pieces)
import           Prelude            hiding (negate, (+))

desmosPrintPW :: PiecewisePoly Rational -> IO ()
desmosPrintPW = putStrLn . desmosShowPW

desmosShowPW :: PiecewisePoly Rational -> String
desmosShowPW pw = unlines $ zipWith desmosShowP (map (:[]) ['a' ..]) (pieces pw)

desmosShowP :: String -> Poly Rational -> String
desmosShowP name (P coeffs) = desmosShowL name coeffs

desmosShowL :: String -> [Rational] -> String
desmosShowL name cs = case desmosShowL' 0 cs of
  "" -> name ++ "(x) = 0"
  s  -> name ++ "(x) = " ++ s

desmosShowL' :: Int -> [Rational] -> String
desmosShowL' _ [] = ""
desmosShowL' d (c : cs)
  | c == 0 = desmosShowL' (d + 1) cs
  | otherwise = case desmosShowL' (d + 1) cs of
    []   -> sign ++ term
    rest -> sign ++ term ++ " " ++ rest
    where
      term = desmosShowRational d c ++ d'
      sign
        | c < 0 = "-"
        | d == 0 = ""
        | otherwise = "+ "
      d'
        | d == 0 = ""
        | d == 1 = "x"
        | otherwise = "x^" ++ show d

desmosShowRational :: Int -> Rational -> String
desmosShowRational d n = term
  where
  n' = if n < 0 then (-n) else n
  a = numerator n'
  b = denominator n'
  term
    | b == 1 = if a == 1 && d /= 0 then "" else show a
    | otherwise = show a ++ "/" ++ show b
