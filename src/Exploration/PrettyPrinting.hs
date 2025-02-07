{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Exploration.PrettyPrinting (
  PrettyBoFun(..),
  prettyPrintPW,
  prettyShowPW,
  prettyPrintP,
  prettyShowP
) where
-- Contains functions for showing polynomials in a way that can be easily copied into
-- a graphical calculator.

import           Data.Ratio         (denominator, numerator)
import           DSLsofMath.Algebra (Additive ((+)), ifThenElse, negate)
import           DSLsofMath.PSDS    (Poly (P))
import           Poly.PiecewisePoly (PiecewisePoly, pieces)
import           Prelude            hiding (negate, (+))

prettyPrintPW :: PiecewisePoly Rational -> IO ()
prettyPrintPW = putStrLn . prettyShowPW

prettyShowPW :: PiecewisePoly Rational -> String
prettyShowPW pw = unlines $ zipWith prettyShowP (map (:[]) ['a' ..]) (pieces pw)

prettyPrintP :: String -> Poly Rational -> IO ()
prettyPrintP name = putStrLn . prettyShowP name

prettyShowP :: String -> Poly Rational -> String
prettyShowP name (P coeffs) = prettyShowL name coeffs

prettyShowL :: String -> [Rational] -> String
prettyShowL name cs = case prettyShowL' 0 cs of
  "" -> name ++ "(x) = 0"
  s  -> name ++ "(x) = " ++ s

prettyShowL' :: Int -> [Rational] -> String
prettyShowL' _ [] = ""
prettyShowL' d (c : cs)
  | c == 0 = prettyShowL' (d + 1) cs
  | otherwise = case prettyShowL' (d + 1) cs of
    []   -> sign ++ term
    rest -> sign ++ term ++ " " ++ rest
    where
      term = prettyShowRational d c ++ d'
      sign
        | c < 0 = "-"
        | d == 0 = ""
        | otherwise = "+ "
      d'
        | d == 0 = ""
        | d == 1 = "x"
        | otherwise = "x^" ++ show d

prettyShowRational :: Int -> Rational -> String
prettyShowRational d n = term
  where
  n' = if n < 0 then (-n) else n
  a = numerator n'
  b = denominator n'
  term
    | b == 1 = if a == 1 && d /= 0 then "" else show a
    | otherwise = show a ++ "/" ++ show b

-- A class for pretty printing BoFuns
-- Lifted and iterated functions use '.' to show the tree structure of the lifted functions.
-- The tree is horizontal, each dot in a line represents moving one step deeper.
class PrettyBoFun f where
  prettyShow :: f -> String

  prettyPrint :: f -> IO ()
  prettyPrint = putStrLn . prettyShow
