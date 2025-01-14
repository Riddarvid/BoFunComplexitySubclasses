{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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

instance BoFun f Int => BoFun (Iterated f) [Int] where
  isConst :: Iterated f -> Maybe Bool
  isConst (Const v)    = Just v
  isConst Id           = Nothing
  isConst (Iterated f) = isConst f

  variables :: Iterated f -> [[Int]]
  variables (Const _)    = []
  variables Id           = [[]]
  variables (Iterated f) = map (uncurry (:)) $ variables f

  setBit :: ([Int], Bool) -> Iterated f -> Iterated f
  setBit _             (Const _)    = error "setBit on const"
  setBit ([], v)       Id           = Const v
  setBit _             Id           = error "Too many levels in path"
  setBit (i : is, val) (Iterated v) = Iterated $ setBit ((i, is), val) v
  setBit ([], _)       (Iterated _) = error "Too few levels in path"
