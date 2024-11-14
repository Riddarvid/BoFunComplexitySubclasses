{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.CanonicalGenFun (
  CanonicalGenFun,
  mkCGF
) where
import           Arity                 (ArbitraryArity (arbitraryArity))
import           BoFun                 (BoFun (..))
import           Control.DeepSeq       (NFData)
import           Data.Function.Memoize (deriveMemoizable)
import           Data.Hashable         (Hashable)
import           GHC.Generics          (Generic)
import           Subclasses.GenFun     (GenFun, toCanonicForm)
import           Test.QuickCheck       (Gen)

newtype CanonicalGenFun = CanonicalGenFun GenFun
  deriving (Generic, Eq, Show)

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

-- We simply use the definition from GenFun, except we flip the result if needed.
instance ArbitraryArity CanonicalGenFun where
  arbitraryArity :: Int -> Gen CanonicalGenFun
  arbitraryArity arity = do
    gf <- arbitraryArity arity
    return $ mkCGF gf
