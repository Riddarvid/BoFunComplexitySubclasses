{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.Iterated (
  Iterated,
  Iterated'
) where
import           BoFun                 (BoFun (..), Constable (mkConst))
import           Control.Monad.Free    (Free (..))
import           Data.Function         ((&))
import           Data.Function.Memoize (Memoizable (memoize), deriveMemoize)
import           Data.Functor.Classes  (Eq1)
import           Data.Hashable         (Hashable)

type Iterated' g f = Free g f

type Iterated g = Iterated' g ()

instance (Eq1 g, Hashable (g (Iterated g))) => Hashable (Iterated g)

instance (BoFun (g (Iterated g)) (i, [i]),
          Constable g) =>
  BoFun (Iterated g) [i] where
  isConst :: Iterated g -> Maybe Bool
  isConst (Pure ()) = Nothing
  isConst (Free u)  = isConst u

  variables :: Iterated g -> [[i]]
  variables (Pure ()) = [[]]
  variables (Free v)  = variables v & map (uncurry (:))

  setBit :: ([i], Bool) -> Iterated g -> Iterated g
  setBit ([], val)     (Pure _) = Free $ mkConst val
  setBit _             (Pure _) = error "Too many levels"
  setBit (i : is, val) (Free v) = Free $ setBit ((i, is), val) v
  setBit ([], _)       (Free _) = error "Too few levels"

instance (Memoizable (f (Iterated f))) => Memoizable (Iterated f) where
  memoize :: (Iterated f -> v) -> Iterated f -> v
  memoize = $(deriveMemoize ''Free)
