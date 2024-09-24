{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.Iterated (
  Iterated
) where
import           BoFun              (BoFun (..))
import           Control.Monad.Free (Free (..))
import           Data.Function      ((&))

type Iterated' g f = Free g f

type Iterated g = Iterated' g ()

class Constable f where
  mkConst :: Bool -> f

instance (BoFun (g (Iterated g)) (Int, [Int]),
          Constable (g (Free g ()))) =>
  BoFun (Iterated g) [Int] where
  isConst :: Iterated g -> Maybe Bool
  isConst (Pure ()) = Nothing
  isConst (Free u)  = isConst u

  variables :: Iterated g -> [[Int]]
  variables (Pure ()) = [[]]
  variables (Free v)  = variables v & map (uncurry (:))

  setBit :: ([Int], Bool) -> Iterated g -> Iterated g
  setBit ([], val)     (Pure _) = Free $ mkConst val
  setBit _             (Pure _) = error "Too many levels"
  setBit (i : is, val) (Free v) = Free $ setBit ((i, is), val) v
  setBit ([], _)       (Free _) = error "Too few levels"
