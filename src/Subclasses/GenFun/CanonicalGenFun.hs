{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- We have chosen to call a BDD canonical if its leftmost path reaches 0.
-- This is equivalent with the output for an input consisting only of 0s being 0.
-- The logic is that flipping all outputs of a BDD should not affect complexity.
module Subclasses.GenFun.CanonicalGenFun (
  CanonicalGenFun,
  mkCGF,
  toCanonicForm
) where
import           Arity                    (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun         (BoFun (..))
import           Control.DeepSeq          (NFData)
import           Data.DecisionDiagram.BDD (AscOrder, BDD, evaluate, notB)
import           Data.Function.Memoize    (deriveMemoizable)
import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)
import           Subclasses.GenFun.GenFun (GenFun (GenFun))
import           Test.QuickCheck          (Gen)

newtype CanonicalGenFun = CanonicalGenFun GenFun
  deriving (Generic, Eq, Show, Read)

$(deriveMemoizable ''CanonicalGenFun)

instance Hashable CanonicalGenFun

instance NFData CanonicalGenFun

mkCGF :: GenFun -> CanonicalGenFun
mkCGF = CanonicalGenFun . toCanonicForm

unlift :: (GenFun -> a) -> CanonicalGenFun -> a
unlift f (CanonicalGenFun gf) = f gf

instance BoFun CanonicalGenFun Int where
  isConst :: CanonicalGenFun -> Maybe Bool
  isConst = unlift isConst
  variables :: CanonicalGenFun -> [Int]
  variables = unlift variables
  setBit :: (Int, Bool) -> CanonicalGenFun -> CanonicalGenFun
  setBit v = CanonicalGenFun . toCanonicForm . unlift (setBit v)

toCanonicForm :: GenFun -> GenFun
toCanonicForm gf@(GenFun bdd n)
  | inCanonicForm bdd = gf
  | otherwise = GenFun (notB bdd) n

inCanonicForm :: BDD AscOrder -> Bool
inCanonicForm = not . evaluate (const False)

-- We simply use the definition from GenFun, except we flip the result if needed.
instance ArbitraryArity CanonicalGenFun where
  arbitraryArity :: Int -> Gen CanonicalGenFun
  arbitraryArity arity = do
    gf <- arbitraryArity arity
    return $ mkCGF gf
