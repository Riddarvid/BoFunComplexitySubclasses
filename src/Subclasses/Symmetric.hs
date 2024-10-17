{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Subclasses.Symmetric (
  BasicSymmetric(BasicSymmetric),
  mkBasicSymmetric,
  Symmetric,
  resultVectorFromList,
  symmResultvector,
  symmSubFuns,
  majFunBasic,
  majFun,
  iteratedFun,
  iteratedMajFun
) where

import           BoFun                 (BoFun (..), Constable (mkConst))
import           Control.Applicative   ((<|>))
import           Control.Arrow         ((>>>))
import           Control.Enumerable    (Enumerable, Shareable, Shared,
                                        Sized (aconcat, pay), Typeable,
                                        enumerate, share)
import           Control.Monad.Free    (Free (Free, Pure))
import           Data.Foldable         (Foldable (toList))
import           Data.Function         ((&))
import           Data.Function.Memoize (Memoizable (memoize), deriveMemoizable)
import           Data.Functor.Classes  (Eq1 (liftEq), Eq2 (liftEq2),
                                        Ord1 (liftCompare), Ord2 (liftCompare2),
                                        Show1 (liftShowsPrec), showsBinaryWith)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NE
import           Data.MultiSet         (MultiSet)
import qualified Data.MultiSet         as MultiSet
import           Data.Sequence         (Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence         as Seq
import           Subclasses.Iterated   (Iterated)
import           Test.QuickCheck       (Arbitrary (arbitrary), chooseInt, sized,
                                        vector)
import           Test.QuickCheck.Gen   (Gen)
import           Utils                 (enumerateMultiSet, naturals, partitions)

--------- BasicSymmetric -----------------------------

newtype BasicSymmetric = BasicSymmetric [Bool]
  deriving (Show, Eq)

instance Arbitrary BasicSymmetric where
  arbitrary :: Gen BasicSymmetric
  arbitrary = sized $ \n -> do
    n' <- (+1) <$> chooseInt(0, n)
    BasicSymmetric <$> vector n'

instance BoFun BasicSymmetric () where
  isConst :: BasicSymmetric -> Maybe Bool
  isConst (BasicSymmetric xs)
    | and xs = Just True
    | all not xs = Just False
    | otherwise = Nothing
  variables :: BasicSymmetric -> [()]
  variables (BasicSymmetric xs)
    | length xs <= 1 = []
    | otherwise = [()]
  setBit :: ((), Bool) -> BasicSymmetric -> BasicSymmetric
  setBit (_, v) (BasicSymmetric xs)
    | v = BasicSymmetric $ tail xs
    | otherwise = BasicSymmetric $ init xs

$(deriveMemoizable ''BasicSymmetric)

-- eval must be defined for [0 .. nBits]
mkBasicSymmetric :: Int -> (Int -> Bool) -> BasicSymmetric
mkBasicSymmetric nBits eval = BasicSymmetric $ map eval [0 .. nBits]

-- Only defined for positive, odd values of n.
majFunBasic :: Int -> BasicSymmetric
majFunBasic n = mkBasicSymmetric n (>= threshold)
  where
    threshold = (n `div` 2) + 1

---------------- Symmetric -----------------------------------------

type Range = Int
type Result = (Bool, Seq Range)

-- A result is defined as a sequence of ints, each describing the number of False/Trues,
-- switching value each time we encounter a new segment.
-- A bool signifies whether we start with a sequence of Falses or Trues.
data Symmetric f = Symmetric {
  symmResultvector :: Result,
  symmSubFuns      :: MultiSet f
} deriving (Show)

resultVectorFromList :: NonEmpty Bool -> Result
resultVectorFromList res@(v :| _) = (v, Seq.fromList $ map NE.length $ NE.group res')
  where
    res' = NE.toList res

instance Memoizable a => Memoizable (Seq a) where
  memoize :: (Seq a -> v) -> Seq a -> v
  memoize f = toList >>> memoize (Seq.fromList >>> f)

instance Eq1 Symmetric where
  liftEq :: (a -> b -> Bool) -> Symmetric a -> Symmetric b -> Bool
  liftEq cmp (Symmetric res1 subs1) (Symmetric res2 subs2)
    = liftEq2 (==) (liftEq cmp) (res1, subs1) (res2, subs2)

instance Ord1 Symmetric where
  liftCompare :: (a -> b -> Ordering) -> Symmetric a -> Symmetric b -> Ordering
  liftCompare cmp (Symmetric res1 subs1) (Symmetric res2 subs2)
    = liftCompare2 compare (liftCompare cmp) (res1, subs1) (res2, subs2)

instance Show1 Symmetric where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Symmetric a -> ShowS
  liftShowsPrec showsPrec' showList' p (Symmetric res subFun) =
    showsBinaryWith showsPrec (liftShowsPrec showsPrec' showList') "SymmetricFun" p res subFun

instance (Ord f, Memoizable f) => Memoizable (Symmetric f) where
  memoize :: (Symmetric f -> a) -> Symmetric f -> a
  memoize f = m >>> memoize (n >>> f) where
    -- Back and forth.
    m (Symmetric rv subFuns) = (rv, subFuns)
    n (rv, subFuns) = Symmetric rv subFuns

instance (Ord f, BoFun f i) => BoFun (Symmetric f) (Int, i) where
  isConst :: Symmetric f -> Maybe Bool
  isConst (Symmetric ranges _) = case ranges of
    (val, _ :<| Empty) -> Just val
    _                  -> Nothing
  variables :: Symmetric f -> [(Int, i)]
  variables (Symmetric _ subFuns) = do
    (i, (u, _)) <- subFuns & MultiSet.toAscOccurList & zip naturals
    v <- variables u
    return (i, v)
  setBit :: ((Int, i), Bool) -> Symmetric f -> Symmetric f
  setBit ((i, v), val) (Symmetric rv subFuns) = case isConst subFun' of
    Just res -> Symmetric rv' subFuns'
      where rv' = if res then removeLowest rv else removeHighest rv
    Nothing -> Symmetric rv $ MultiSet.insert subFun' subFuns'
    where
    (subFun, _) = MultiSet.toAscOccurList subFuns !! i
    subFuns' = subFuns & MultiSet.delete subFun
    subFun' = setBit (v, val) subFun

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

instance Constable Symmetric where
  mkConst :: Bool -> Symmetric f
  mkConst val = Symmetric (val, Seq.singleton 1) MultiSet.empty

-- Examples:

majFun :: Int -> Symmetric (Maybe Bool)
majFun n = Symmetric (False, Seq.fromList [n', n']) $ MultiSet.fromOccurList [(Nothing, n)]
  where
    n' = (n `div` 2) + 1

----------- Iterated Symmetric -------------------------

iteratedFun :: [(Int, Result)] -> Iterated Symmetric
iteratedFun [] = Pure ()
iteratedFun ((nBits, res) : ress) = Free $
  Symmetric res $ MultiSet.fromOccurList [(subFun, nBits)]
  where
    subFun = iteratedFun ress

iteratedMajFun :: Int -> Int -> Iterated Symmetric
iteratedMajFun nBits numStages = replicate numStages nBits & iteratedMajFun'

iteratedMajFun' :: [Int] -> Iterated Symmetric
iteratedMajFun' = iteratedFun . map (\nBits -> (nBits, majSequence nBits))

majSequence :: Int -> Result
majSequence nBits = (False, Seq.fromList [votes, votes])
  where
    votes = (nBits + 1) `div` 2

-------------------- Enumeration ------------------------

-- We iterate over the number of subfunctions
instance (Enumerable g, Ord g) => Enumerable (Symmetric g) where
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
  return (v, Seq.fromList seqList)

