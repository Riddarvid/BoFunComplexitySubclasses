{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.Symmetric (main, symmMaj) where

import           Algorithm.GenAlgPW    (computeMin)
import           BoFun                 (BoFun (..))
import           Data.Function.Memoize (deriveMemoizable)
import           Poly.PiecewisePoly    (showPW)


newtype Symmetric = Symmetric [Bool]
  deriving Show

instance BoFun Symmetric () where
  isConst :: Symmetric -> Maybe Bool
  isConst (Symmetric xs)
    | and xs = Just True
    | all not xs = Just False
    | otherwise = Nothing
  variables :: Symmetric -> [()]
  variables (Symmetric xs)
    | length xs <= 1 = []
    | otherwise = [()]
  setBit :: ((), Bool) -> Symmetric -> Symmetric
  setBit (_, v) (Symmetric xs)
    | v = Symmetric $ tail xs
    | otherwise = Symmetric $ init xs

$(deriveMemoizable ''Symmetric)

-- Examples

-- Only defined for positive, odd values of n.
symmMaj :: Int -> Symmetric
symmMaj n = mkSymmetric n (>= threshold)
  where
    threshold = (n `div` 2) + 1

-- eval must be defined for [0 .. nBits]
mkSymmetric :: Int -> (Int -> Bool) -> Symmetric
mkSymmetric nBits eval = Symmetric $ map eval [0 .. nBits]

sumMod :: Symmetric
sumMod = mkSymmetric 70 (\n -> n `mod` 2 == 1)

main :: IO ()
main = do
  putStrLn $ "sumMod2, 1 bit: " ++ showPW (computeMin sumMod)
