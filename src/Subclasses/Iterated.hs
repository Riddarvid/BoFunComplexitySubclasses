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
import           Control.Enumerable    (Enumerable, Shared, Sized (aconcat),
                                        Typeable, c1, share)
import           Control.Monad.Free    (Free (..))
import           Data.Function         ((&))
import           Data.Function.Memoize (Memoizable (memoize), deriveMemoize)
import           Data.Functor.Classes  (Eq1)
import           Data.Hashable         (Hashable)
import           Test.Feat             (enumerate)

type Iterated' f g = Free f g

type Iterated f = Iterated' f ()

instance (Eq1 f, Hashable g, Hashable (f (Iterated' f g))) => Hashable (Iterated' f g)

instance (BoFun (f (Iterated f)) (i, [i]),
          Constable f) =>
  BoFun (Iterated f) [i] where
  isConst :: Iterated f -> Maybe Bool
  isConst (Pure ()) = Nothing
  isConst (Free u)  = isConst u

  variables :: Iterated f -> [[i]]
  variables (Pure ()) = [[]]
  variables (Free v)  = variables v & map (uncurry (:))

  setBit :: ([i], Bool) -> Iterated f -> Iterated f
  setBit ([], val)     (Pure _) = Free $ mkConst val
  setBit _             (Pure _) = error "Too many levels"
  setBit (i : is, val) (Free v) = Free $ setBit ((i, is), val) v
  setBit ([], _)       (Free _) = error "Too few levels"

-- f will likely be memoizable over all types of subfunction,
-- but here we only need it to be memoizable over specifically f (Iterated f)
instance (Memoizable (f (Iterated' f g)), Memoizable g) => Memoizable (Iterated' f g) where
  memoize :: (Iterated' f g -> v) -> Iterated' f g -> v
  memoize = $(deriveMemoize ''Free)

instance (Enumerable g, Enumerable (f (Iterated' f g)), Typeable f) =>
  Enumerable (Iterated' f g) where

  enumerate :: (Typeable enum, Sized enum) => Shared enum (Iterated' f g)
  enumerate = share $ aconcat [
    c1 Pure,
    c1 Free]
