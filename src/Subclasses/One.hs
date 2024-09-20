{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.One (
  One
) where
import           BoFun                 (BoFun (..))
import           Data.Function.Memoize (deriveMemoizable)

-- Work in progress

-- For an n-bit function, the list of bools has n elements.
-- One [] represent a constant function with the value True
-- OFalse represents a constant function with the value False
data One = One [Bool] | OFalse
  deriving (Show)

$(deriveMemoizable ''One)

instance BoFun One Int where
  isConst :: One -> Maybe Bool
  isConst OFalse   = Just False
  isConst (One []) = Just True
  isConst _        = Nothing
  variables :: One -> [Int]
  variables (One pattern) = zipWith const [0 ..] pattern
  variables OFalse        = []
  setBit :: (Int, Bool) -> One -> One
  setBit (i, v) (One pattern)
    | pattern !! i == v = One $ deleteAt i pattern
    | otherwise = OFalse
    where
      deleteAt :: Int -> [a] -> [a]
      deleteAt _ []       = []
      deleteAt 0 (_ :xs)  = xs
      deleteAt n (x : xs) = x : deleteAt (n - 1) xs
  setBit _ OFalse = OFalse

-- TODO-NEW mkOne
