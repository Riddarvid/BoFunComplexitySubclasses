module Exploration.Eval (evalSymmetric) where
import           BoFun (BoFun (..))

-- The caller is responsible for ensuring that the function is symmetric.
evalSymmetric :: BoFun f i => f -> Int -> Bool
evalSymmetric f n = case isConst f of
  Just v -> v
  Nothing -> evalSymmetric (setBit (i, v) f) (n - 1)
    where
      i = head $ variables f
      v = n > 0
