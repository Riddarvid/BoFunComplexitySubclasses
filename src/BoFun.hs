{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module BoFun (
  BoFun(..),
  viewConst,
  outgoing,
  reachable,
  eval
) where

import qualified Data.Set as Set
import           Utils    (dfs)

class BoFun f i | f -> i where
  isConst   :: f -> Maybe Bool
  variables :: f -> [i]
  setBit    :: (i, Bool) -> f -> f

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

eval :: BoFun f i => f -> [(i, Bool)] -> Maybe Bool
eval f vals = case isConst f of
  Just res -> Just res
  Nothing -> case vals of
    []       -> Nothing
    (x : xs) -> eval (setBit x f) xs
