{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Simplified version of Iterated used in the report.
-- TODO-NEW make sure that the latest version is in the report

module Subclasses.Iterated.IteratedSimplified () where
import           Complexity.BoFun  (BoFun (..))
import           Subclasses.Lifted (Lifted)

type MultiComposed f g = Lifted f g
type SubFun f = MultiComposed f (Iterated f)
data Iterated f = Const Bool | Id | Iterated (SubFun f)

instance (BoFun (SubFun f) (i, [i])) => BoFun (Iterated f) [i] where
  isConst :: Iterated f -> Maybe Bool
  isConst (Const v)    = Just v
  isConst Id           = Nothing
  isConst (Iterated f) = isConst f

  variables :: Iterated f -> [[i]]
  variables (Const _)    = []
  variables Id           = [[]]
  variables (Iterated f) = map (uncurry (:)) $ variables f

  setBit :: ([i], Bool) -> Iterated f -> Iterated f
  setBit _             (Const _)    = error "setBit on const"
  setBit ([], v)       Id           = Const v
  setBit _             Id           = error "Too many levels in path"
  setBit (i : is, val) (Iterated v) = Iterated $ setBit ((i, is), val) v
  setBit ([], _)       (Iterated _) = error "Too few levels in path"
