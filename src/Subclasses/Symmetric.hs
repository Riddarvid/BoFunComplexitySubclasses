{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
module Subclasses.Symmetric (
  SymmetricFun,
  NonSymmSymmetricFun(NonSymmSymmetricFun),
  mkSymmetricFun,
  arity,
  majFun,
  iteratedMajFun
) where

import           Arity                        (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun             (BoFun (..), Constable (mkConst))
import           Control.Arrow                ((>>>))
import           Control.DeepSeq              (NFData)
import           Data.Foldable                (Foldable (toList))
import           Data.Function.Memoize        (Memoizable (memoize),
                                               deriveMemoizable)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.List.NonEmpty           as NE
import           Data.Sequence                (Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence                as Seq
import           GHC.Generics                 (Generic)
import           Subclasses.Iterated.Iterated (Iterated, iterateFun)
import           Test.QuickCheck              (Arbitrary (arbitrary), chooseInt,
                                               sized, vector)
import           Test.QuickCheck.Gen          (Gen)
import           Utils                        (naturals)

--------- Ranges -----------------------------

-- A result is defined as a sequence of ints, each describing the number of False/Trues,
-- switching value each time we encounter a new segment.
-- A bool signifies whether we start with a sequence of Falses or Trues.
type Range = Int
type Result = (Bool, Seq Range)

resultVectorFromList :: NonEmpty Bool -> Result
resultVectorFromList res@(v :| _) = (v, Seq.fromList $ map NE.length $ NE.group res')
  where
    res' = NE.toList res

removeLowest :: Result -> Result
removeLowest (v, length' :<| xs)
  | length' == 1 = (not v, xs)
  | otherwise = (v, length' - 1 :<| xs)
removeLowest (_, Empty) = undefined

removeHighest :: Result -> Result
removeHighest (v, xs :|> length')
  | length' == 1 = (v, xs)
  | otherwise = (v, xs :|> length' - 1)
removeHighest (_, Empty) = undefined

resultLength :: Result -> Int
resultLength (_, xs) = sum xs

instance Memoizable a => Memoizable (Seq a) where
  memoize :: (Seq a -> v) -> Seq a -> v
  memoize f = toList >>> memoize (Seq.fromList >>> f)

--------- Symmetric Functions ---------------------

newtype SymmetricFun = SymmetricFun Result
  deriving (Show, Eq, Ord, Generic)

instance NFData SymmetricFun

-- The input vector represents the result for zero 1's, one 1, two 1's etc.
mkSymmetricFun :: NonEmpty Bool -> SymmetricFun
mkSymmetricFun = SymmetricFun . resultVectorFromList

arity :: SymmetricFun -> Int
arity (SymmetricFun res) = resultLength res - 1

$(deriveMemoizable ''SymmetricFun)

instance Arbitrary SymmetricFun where
  arbitrary :: Gen SymmetricFun
  arbitrary = sized $ \n -> do
    arity' <- chooseInt (0, n)
    arbitraryArity arity'

instance ArbitraryArity SymmetricFun where
  arbitraryArity :: Int -> Gen SymmetricFun
  arbitraryArity arity' = do
    SymmetricFun . resultVectorFromList . NE.fromList <$> vector (arity' + 1)

instance BoFun SymmetricFun () where
  isConst :: SymmetricFun -> Maybe Bool
  isConst (SymmetricFun ranges) = case ranges of
    (val, _ :<| Empty) -> Just val
    _                  -> Nothing
  variables :: SymmetricFun -> [()]
  variables f@(SymmetricFun _) = case isConst f of
    Nothing -> [()]
    Just _  -> []
  setBit :: ((), Bool) -> SymmetricFun -> SymmetricFun
  setBit (_, val) (SymmetricFun rv) = SymmetricFun rv'
    where
      rv' = if val then removeLowest rv else removeHighest rv

instance Constable SymmetricFun where
  mkConst :: Bool -> SymmetricFun
  mkConst val = SymmetricFun (val, Seq.singleton 1)

newtype NonSymmSymmetricFun = SymmetricFun' SymmetricFun
  deriving (Memoizable, NFData, ArbitraryArity)

pattern NonSymmSymmetricFun :: Result -> NonSymmSymmetricFun
pattern NonSymmSymmetricFun f = SymmetricFun' (SymmetricFun f)

instance BoFun NonSymmSymmetricFun Int where
  isConst :: NonSymmSymmetricFun -> Maybe Bool
  isConst (SymmetricFun' f) = isConst f
  variables :: NonSymmSymmetricFun -> [Int]
  variables (SymmetricFun' f) = take (arity f) naturals
  setBit :: (Int, Bool) -> NonSymmSymmetricFun -> NonSymmSymmetricFun
  setBit (_, v) (SymmetricFun' f) = SymmetricFun' $ setBit ((), v) f

----------------- Examples -----------------------

majFun :: Int -> SymmetricFun
majFun bits = SymmetricFun (False, Seq.fromList [n, n])
  where
    n = (bits `div` 2) + 1

iteratedMajFun :: Int -> Int -> Iterated NonSymmSymmetricFun
iteratedMajFun bits = iterateFun bits (SymmetricFun' $ majFun bits)

-------------------- Enumeration ------------------------

-- TODO-NEW generalize and move to Iterated

-- We iterate over the number of subfunctions
{-instance (Enumerable g, Ord g) => Enumerable (Symmetric g) where
  enumerate :: (Typeable f, Sized f) => Shared f (Symmetric g)
  enumerate = share $ go 0
    where
      go n = pay $ enumerateSymmetric n <|> go (n + 1)

enumerateSymmetric :: (Typeable f, Sized f, Ord g, Enumerable g) => Int -> Shareable f (Symmetric g)
enumerateSymmetric n = Symmetric <$> enumerateResults n <*> enumerateMultiSet n

enumerateResults :: (Typeable f, Sized f) => Int -> Shareable f Result
enumerateResults n = aconcat $ map pure $ do
  v <- [False, True]
  seqList <- partitions (n + 1)
  return (v, Seq.fromList seqList)-}

