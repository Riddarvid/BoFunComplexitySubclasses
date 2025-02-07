{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.MultiComposed.MultiComposed (
  MultiComposed(MultiComposed),
  multiCompose
) where
import           Arity                      (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun           (BoFun (..), shrinkBoFun)
import           Control.DeepSeq            (NFData)
import           Data.Maybe                 (fromJust)
import           Exploration.PrettyPrinting (PrettyBoFun (prettyShow))
import           GHC.Generics               (Generic)
import           Test.QuickCheck            (Arbitrary (shrink), Gen, chooseInt,
                                             sized)
import           Test.QuickCheck.Arbitrary  (arbitrary)
import           Utils                      (generatePartition, indent,
                                             naturals)

-- Invariant: g only contains non-const functions
data MultiComposed f g = MultiComposed' {
  mcFun     :: f,
  mcSubFuns :: [g]
} deriving (Generic, Show, Read)

pattern MultiComposed :: (BoFun f Int, BoFun g i) => f -> [g] -> MultiComposed f g
pattern MultiComposed f gs <- MultiComposed' f gs where
  MultiComposed f gs = multiCompose f gs

multiCompose :: (BoFun f Int, BoFun g i) => f -> [g] -> MultiComposed f g
multiCompose f = reduceConstants . MultiComposed' f

instance (PrettyBoFun f, PrettyBoFun g) => PrettyBoFun (MultiComposed f g) where
  prettyShow :: MultiComposed f g -> String
  prettyShow (MultiComposed' f gs) =
    prettyShow f ++ "\n" ++
    indent (showSubFuns gs)

showSubFuns :: PrettyBoFun g => [g] -> String
showSubFuns = unlines . map prettyShow

instance (NFData f, NFData g) => NFData (MultiComposed f g)

reduceConstants :: (BoFun f Int, BoFun g i) => MultiComposed f g -> MultiComposed f g
reduceConstants (MultiComposed' f gs) = case v of
  Nothing      -> MultiComposed' f gs
  Just (i, v') -> reduceConstants $ MultiComposed' (setBit (i, v') f) (deleteAt i gs)
  where
    v = firstConst $ zip naturals $ map isConst gs

firstConst :: [(Int, Maybe a)] -> Maybe (Int, a)
firstConst [] = Nothing
firstConst ((i, v) : vs) = case v of
  Nothing -> firstConst vs
  Just v' -> Just (i, v')

instance (BoFun f Int, BoFun g j) => BoFun (MultiComposed f g) (Int, j) where
  isConst :: MultiComposed f g -> Maybe Bool
  isConst = isConst . mcFun
  variables :: MultiComposed f g -> [(Int, j)]
  variables lf = do
    (subFun, i) <- zip (mcSubFuns lf) naturals
    j <- variables subFun
    return (i, j)
  setBit :: ((Int, j), Bool) -> MultiComposed f g -> MultiComposed f g
  setBit ((i, j), v) lf = case isConst subFun' of
    Nothing -> lf{mcSubFuns = start ++ (subFun' : end)}
    Just v' -> lf{mcFun = setBit (i, v') $ mcFun lf, mcSubFuns = start ++ end}
    where
      (start, subFun, end) = fromJust $ splitList i $ mcSubFuns lf
      subFun' = setBit (j, v) subFun

splitList :: Int -> [a] -> Maybe ([a], a, [a])
splitList n xs = case end of
  []        -> Nothing
  (x: end') -> Just (start, x, end')
  where
    (start, end) = splitAt n xs

deleteAt :: Int -> [a] -> [a]
deleteAt _ []       = []
deleteAt 0 (_ : xs) = xs
deleteAt n (_ : xs) = deleteAt (n - 1) xs

instance (ArbitraryArity f, ArbitraryArity g, BoFun f Int, BoFun g j) => Arbitrary (MultiComposed f g) where
  arbitrary :: Gen (MultiComposed f g)
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    arbitraryArity n'
  shrink :: MultiComposed f g -> [MultiComposed f g]
  shrink = shrinkBoFun

instance (ArbitraryArity f, ArbitraryArity g) => ArbitraryArity (MultiComposed f g) where
  arbitraryArity :: Int -> Gen (MultiComposed f g)
  arbitraryArity arity = do
    (gs, nSubFuns) <- generateSubFuns arity
    f <- arbitraryArity nSubFuns
    return $ MultiComposed' {mcFun = f, mcSubFuns = gs}

generateSubFuns :: (ArbitraryArity f) => Int -> Gen ([f], Int)
generateSubFuns totalArity = do
  partition <- generatePartition totalArity
  subFuns <- mapM arbitraryArity partition
  return (subFuns, length subFuns)
