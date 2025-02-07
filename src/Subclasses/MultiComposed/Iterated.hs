{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.MultiComposed.Iterated (
  Iterated'(Const, Id, Iterated),
  SubFun,
  Iterated,
  iterateFun
) where
import           Arity                      (AllArity (allArity),
                                             ArbitraryArity (arbitraryArity))
import           Complexity.BoFun           (BoFun (..), Constable (mkConst))
import           Control.DeepSeq            (NFData)
import           Control.Enumerable         (Enumerable, Shared, Sized,
                                             Typeable, c0, c1, datatype)
import           Data.Hashable              (Hashable)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified DSLsofMath.Algebra         as A
import           Exploration.PrettyPrinting (PrettyBoFun (prettyShow))
import           GHC.Generics               (Generic)
import           Subclasses.MultiComposed.MultiComposed          (Lifted (Lifted))
import           Test.Feat                  (enumerate)
import           Test.QuickCheck            (Arbitrary (arbitrary, shrink), Gen,
                                             chooseInt, oneof, sized)

type SubFun f = f (Iterated' f)

data Iterated' f = Const Bool | Id | Iterated' (SubFun f)
  deriving (Generic)

deriving instance (Show (SubFun f)) => Show (Iterated' f)

deriving instance (Read (SubFun f)) => Read (Iterated' f)

instance (PrettyBoFun (SubFun f)) => PrettyBoFun (Iterated' f) where
  prettyShow :: Iterated' f -> String
  prettyShow (Const False) = "F"
  prettyShow (Const True)  = "T"
  prettyShow Id            = "x"
  prettyShow (Iterated' f) = prettyShow f

instance (NFData (SubFun f)) => NFData (Iterated' f)

-- Some functions that are equal could still result in False,
-- but this should not be an issue.
instance (Eq (SubFun f)) => Eq (Iterated' f) where
  (==) :: Iterated' f -> Iterated' f -> Bool
  (Const v1) == (Const v2)     = v1 == v2
  Id == Id                     = True
  Iterated' f1 == Iterated' f2 = f1 == f2
  _ == _                       = False

instance Ord (SubFun f) => Ord (Iterated' f) where
  compare :: Iterated' f -> Iterated' f -> Ordering
  compare (Const v1)    (Const v2)      = compare v1 v2
  compare (Const _)     _               = LT
  compare _             (Const _)       = GT
  compare Id            Id              = EQ
  compare (Iterated' f1) (Iterated' f2) = compare f1 f2
  compare (Iterated' _)  _              = GT
  compare _             (Iterated' _)   = LT

-- type IteratedSymm f = Iterated' (LiftedSymmetric f)

--------------------------------------------

instance (Hashable (SubFun f)) => Hashable (Iterated' f)

instance (BoFun (SubFun f) (i, [i])) => BoFun (Iterated' f) [i] where
  isConst :: Iterated' f -> Maybe Bool
  isConst (Const v)     = Just v
  isConst Id            = Nothing
  isConst (Iterated' f) = isConst f

  variables :: Iterated' f -> [[i]]
  variables (Const _)     = []
  variables Id            = [[]]
  variables (Iterated' f) = map (uncurry (:)) $ variables f

  setBit :: ([i], Bool) -> Iterated' f -> Iterated' f
  setBit _             (Const _)     = error "setBit on const"
  setBit ([], v)       Id            = Const v
  setBit _             Id            = error "Too many levels in path"
  setBit (i : is, val) (Iterated' v) = Iterated' $ setBit ((i, is), val) v
  setBit ([], _)       (Iterated' _) = error "Too few levels in path"

-- TODO-NEW: This probably works as a general instance, but we should also look at
-- something using bit-number.
instance (Enumerable (SubFun f), Typeable f) => Enumerable (Iterated' f) where
  enumerate :: (Typeable enum, Sized enum) => Shared enum (Iterated' f)
  enumerate = datatype [
    c1 Const,
    c0 Id,
    c1 Iterated']

instance Constable (Iterated' f) where
  mkConst :: Bool -> Iterated' f
  mkConst = Const

-- Size is used to determine where the tree should end
instance (Arbitrary (SubFun f), ArbitraryArity (SubFun f)) => Arbitrary (Iterated' f) where
  arbitrary :: Gen (Iterated' f)
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    arbitraryArity n'
  shrink :: Iterated' f -> [Iterated' f]
  shrink (Iterated' f) = [Const False, Const True, Id] ++ map Iterated' (shrink f)
  shrink _ = []

instance (ArbitraryArity (SubFun f)) => ArbitraryArity (Iterated' f) where
  arbitraryArity :: Int -> Gen (Iterated' f)
  arbitraryArity arity = oneof (subFunGen : gens)
    where
      subFunGen = Iterated' <$> arbitraryArity arity
      gens = case arity of
        0 -> [Const <$> arbitrary]
        1 -> [pure Id]
        _ -> []

instance (AllArity (SubFun f), Ord (SubFun f)) => AllArity (Iterated' f) where
  allArity :: Int -> Set (Iterated' f)
  allArity n = case n of
    0 -> Set.insert (Const False) $ Set.insert (Const True) subFuns
    1 -> Set.insert Id subFuns
    _ -> subFuns
    where
      subFuns = Set.map Iterated' $ allArity n

type Iterated f = Iterated' (Lifted f)

pattern Iterated :: BoFun f Int => f -> [Iterated f] -> Iterated f
pattern Iterated f gs = Iterated' (Lifted f gs)

iterateFun :: (BoFun f Int) => Int -> f -> Int -> Iterated f
iterateFun bits f = go
  where
    go 0 = Id
    go levels = Iterated f $ replicate bits subFun
      where
        subFun = iterateFun bits f (levels A.- 1)
