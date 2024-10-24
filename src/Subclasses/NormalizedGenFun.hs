{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.NormalizedGenFun (
  NormalizedGenFun,
  mkNGF,
  ngfBDD,
  ngfArity
) where
import           BDD.BDD                  (normalizeBDD)
import           BoFun                    (BoFun (..))
import           Data.DecisionDiagram.BDD (AscOrder, BDD)
import           Data.Function.Memoize    (deriveMemoizable)
import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)
import           Subclasses.GenFun        (GenFun (GenFun), toCanonicForm)

newtype NormalizedGenFun = NormalizedGenFun GenFun
  deriving (Generic, Eq)

$(deriveMemoizable ''NormalizedGenFun)

instance Hashable NormalizedGenFun

mkNGF :: GenFun -> NormalizedGenFun
mkNGF gf = NormalizedGenFun (normalizeGenFun gf)

unlift :: (GenFun -> a) -> NormalizedGenFun -> a
unlift f (NormalizedGenFun gf) = f gf

ngfBDD :: NormalizedGenFun -> BDD AscOrder
ngfBDD (NormalizedGenFun (GenFun bdd _)) = bdd

ngfArity :: NormalizedGenFun -> Int
ngfArity (NormalizedGenFun (GenFun _ n)) = n

instance BoFun  NormalizedGenFun Int where
  isConst :: NormalizedGenFun -> Maybe Bool
  isConst = unlift isConst
  variables :: NormalizedGenFun -> [Int]
  variables = variablesNGF
  setBit :: (Int, Bool) -> NormalizedGenFun -> NormalizedGenFun
  setBit v = normalize . (NormalizedGenFun . unlift (setBit v))

variablesNGF :: NormalizedGenFun -> [Int]
variablesNGF ngf = [1 .. ngfArity ngf]

normalize :: NormalizedGenFun -> NormalizedGenFun
normalize = NormalizedGenFun . unlift (toCanonicForm . normalizeGenFun)

------------------- Normalizing variable indeces ----------------

normalizeGenFun :: GenFun -> GenFun
normalizeGenFun (GenFun bdd _) = GenFun bdd' n'
  where
    (bdd', n') = normalizeBDD bdd
