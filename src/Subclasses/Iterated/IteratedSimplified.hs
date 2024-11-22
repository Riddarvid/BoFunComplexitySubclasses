{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- Simplified version of Iterated used in the report.
module Subclasses.Iterated.IteratedSimplified () where
import           BoFun (BoFun (..))

type SubFun f = f (Iterated f)

data Iterated f = Const Bool | Id | Iterated (SubFun f)

instance (BoFun (SubFun f) (i, [i])) => BoFun (Iterated f) [i] where
  isConst :: Iterated f -> Maybe Bool
  isConst (Const v)    = Just v
  isConst Id           = Nothing
  isConst (Iterated f) = isConst f

  -- The variables are represented as search paths through the tree
  -- represented by the Iterated-type.
  -- A const function has no variables,
  -- an id function has only a single path, consisting of moving nowhere,
  -- paths for iterated functions are computed recursively.
  variables :: Iterated f -> [[i]]
  variables (Const _)    = []
  variables Id           = [[]]
  variables (Iterated f) = map (uncurry (:)) $ variables f

  setBit :: ([i], Bool) -> Iterated f -> Iterated f
  setBit _             (Const _)    = error "setBit on const"
  setBit ([], v)       Id           = Const v
  setBit _             Id           = error "Too many levels in path"
  -- We could add a check here to see if the sub function is const
  -- and if so replace it with Const.
  setBit (i : is, val) (Iterated v) = Iterated $ setBit ((i, is), val) v
  setBit ([], _)       (Iterated _) = error "Too few levels in path"
