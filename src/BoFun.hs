{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module BoFun (
  BoFun(..),
  viewConst,
  outgoing,
  reachable,
  Constable(mkConst),
  shrinkFun
) where

import qualified Data.Set as Set
import           Utils    (dfs)

class BoFun f i | f -> i where
  isConst   :: f -> Maybe Bool
  variables :: f -> [i]
  setBitAndNormalize :: (i, Bool) -> f -> f
  setBitAndNormalize v = normalize . setBit v
  setBit    :: (i, Bool) -> f -> f
  normalize :: f -> f
  normalize = id

viewConst :: BoFun f i => f -> Either Bool f
viewConst f = maybe (Right f) Left (isConst f)

-- For a BoFun f, for each variable generates two new BoFuns by setting that variable to either
-- True or False.
outgoing :: (BoFun f i) => f -> [f]
outgoing u = do
  v <- variables u
  val <- [True, False]
  return $ setBit (v, val) u

-- Recursively generates the set of all sub-BoFuns of f.
reachable :: (Ord f, BoFun f i) => f -> Set.Set f
reachable = dfs outgoing

class Constable f where
  mkConst :: Bool -> f g

shrinkFun :: BoFun f i => f -> [f]
shrinkFun f = case vars of
  []    -> []
  vars' -> [setBit (v, val) f | v <- vars', val <- [False, True]]
  where
    vars = variables f
