{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.GenFun.MinimizedGenFun (
  MinimizedGenFun,
  mkNGF,
  ngfBDD,
  ngfArity,
  minimizeGenFun
) where
import           Arity                      (ArbitraryArity (arbitraryArity))
import           BDD.BDD                    (minimizeBDD)
import           Complexity.BoFun           (BoFun (..))
import           Control.DeepSeq            (NFData)
import           Data.DecisionDiagram.BDD   (AscOrder, BDD)
import           Data.Function.Memoize      (deriveMemoizable)
import           Data.Hashable              (Hashable)
import           Exploration.PrettyPrinting (PrettyBoFun (prettyShow))
import           GHC.Generics               (Generic)
import           Subclasses.GenFun.GenFun   (GenFun (GenFun))
import           Test.QuickCheck            (Gen)

newtype MinimizedGenFun = MinimizedGenFun GenFun
  deriving (Generic, Eq, Show, Read)

$(deriveMemoizable ''MinimizedGenFun)

instance Hashable MinimizedGenFun

instance NFData MinimizedGenFun

mkNGF :: GenFun -> MinimizedGenFun
mkNGF = MinimizedGenFun . minimizeGenFun

unlift :: (GenFun -> a) -> MinimizedGenFun -> a
unlift f (MinimizedGenFun gf) = f gf

ngfBDD :: MinimizedGenFun -> BDD AscOrder
ngfBDD (MinimizedGenFun (GenFun bdd _)) = bdd

ngfArity :: MinimizedGenFun -> Int
ngfArity (MinimizedGenFun (GenFun _ n)) = n

instance BoFun  MinimizedGenFun Int where
  isConst :: MinimizedGenFun -> Maybe Bool
  isConst = unlift isConst
  variables :: MinimizedGenFun -> [Int]
  variables = variablesNGF
  setBit :: (Int, Bool) -> MinimizedGenFun -> MinimizedGenFun
  setBit v = MinimizedGenFun . minimizeGenFun . unlift (setBit v)

variablesNGF :: MinimizedGenFun -> [Int]
variablesNGF ngf = [1 .. ngfArity ngf]

minimizeGenFun :: GenFun -> GenFun
minimizeGenFun (GenFun bdd _) = GenFun bdd' n'
  where
    (bdd', n') = minimizeBDD bdd

-- The resulting NGF will not necessarily be an n-bit function. However, it
-- is the minimized version of an n-bit GenFun, which is what we're comparing.
instance ArbitraryArity MinimizedGenFun where
  arbitraryArity :: Int -> Gen MinimizedGenFun
  arbitraryArity arity = do
    gf <- arbitraryArity arity
    return $ mkNGF gf

instance PrettyBoFun MinimizedGenFun where
  prettyShow :: MinimizedGenFun -> String
  prettyShow (MinimizedGenFun f@(GenFun _ n)) = "Minimized general function with arity " ++ show n ++ "\n" ++ prettyShow f
