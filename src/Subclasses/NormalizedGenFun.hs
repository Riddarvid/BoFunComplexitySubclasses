{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.NormalizedGenFun (
  NormalizedGenFun,
  mkNGF,
  ngfBDD,
  ngfArity,
  normalizeGenFun
) where
import           Arity                    (ArbitraryArity (arbitraryArity))
import           BDD.BDD                  (normalizeBDD)
import           BoFun                    (BoFun (..))
import           Control.DeepSeq          (NFData)
import           Data.DecisionDiagram.BDD (AscOrder, BDD)
import           Data.Function.Memoize    (deriveMemoizable)
import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)
import           Subclasses.GenFun        (GenFun (GenFun))
import           Test.QuickCheck          (Gen)

newtype NormalizedGenFun = NormalizedGenFun GenFun
  deriving (Generic, Eq, Show)

$(deriveMemoizable ''NormalizedGenFun)

instance Hashable NormalizedGenFun

instance NFData NormalizedGenFun

mkNGF :: GenFun -> NormalizedGenFun
mkNGF = NormalizedGenFun . normalizeGenFun

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
  setBit v = NormalizedGenFun . normalizeGenFun . unlift (setBit v)

variablesNGF :: NormalizedGenFun -> [Int]
variablesNGF ngf = [1 .. ngfArity ngf]

normalizeGenFun :: GenFun -> GenFun
normalizeGenFun (GenFun bdd _) = GenFun bdd' n'
  where
    (bdd', n') = normalizeBDD bdd

-- The resulting NGF will not necessarily be an n-bit function. However, it
-- is the normalized version of an n-bit GenFun, which is what we're comparing.
instance ArbitraryArity NormalizedGenFun where
  arbitraryArity :: Int -> Gen NormalizedGenFun
  arbitraryArity arity = do
    gf <- arbitraryArity arity
    return $ mkNGF gf
