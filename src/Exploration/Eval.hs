-- Code for evaluating Symmetric, as well as non-symmetric functions.
-- Assumes that variable ordering is consistent.

module Exploration.Eval (
  evalSymmetric,
  evalNonSymmetric
) where
import           Complexity.BoFun (BoFun (..))

evalNonSymmetric :: BoFun f i => f -> [Bool] -> Maybe Bool
evalNonSymmetric f input = case isConst f of
  Just v -> Just v
  Nothing -> case input of
    []       -> Nothing
    (v : vs) -> evalNonSymmetric (setBit (i, v) f) vs
  where
    i = head $ variables f


-- The caller is responsible for ensuring that the function is symmetric.
evalSymmetric :: BoFun f i => f -> Int -> Bool
evalSymmetric f n = case isConst f of
  Just v -> v
  Nothing -> evalSymmetric (setBit (i, v) f) (n - 1)
    where
      i = head $ variables f
      v = n > 0
