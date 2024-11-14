{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Timing (
  measureMajs,
  measureFuns,
  measureRandomFuns
) where
import           Arity           (ArbitraryArity (arbitraryArity))
import           Control.Monad   (replicateM)
import           Data.List       (sort)
import           Data.Time       (NominalDiffTime)
import           Test.QuickCheck (generate)

measureMajs :: (f -> IO NominalDiffTime) -> (Int -> f) -> Int -> Int -> IO [(Int, NominalDiffTime)]
measureMajs timingFun majFun samples n = zip bits <$> measureFuns timingFun funs samples
  where
    bits = [1, 3 .. n]
    funs = map majFun bits

measureFuns :: (f -> IO NominalDiffTime) -> [f] -> Int -> IO [NominalDiffTime]
measureFuns timingFun funs samples = sequenceA measurements
  where
    measurements = map (medianMeasure timingFun samples) funs

medianMeasure :: (f -> IO NominalDiffTime) -> Int -> f -> IO NominalDiffTime
medianMeasure timingFun n f = do
  times <- sequenceA timingActions
  return $ median times
  where
    timingActions = replicate n (timingFun f)

median :: (Ord a, Fractional a) => [a] -> a
median times
  | odd lt = times' !! mid
  | otherwise = (times' !! mid' + times' !! mid) / 2
  where
    times' = sort times
    lt = length times'
    mid = lt `div` 2
    mid' = mid - 1

type NDT = NominalDiffTime

measureRandomFuns :: ArbitraryArity f =>
  (f -> IO NominalDiffTime) -> Int -> Int -> Int -> IO (NDT, NDT, NDT, NDT, NDT)
measureRandomFuns timingFun nFuns nSamples arity = do
  funs <- genFuns
  measureFiveValues timingFun nSamples funs
  where
    genFun = generate $ arbitraryArity arity
    genFuns = replicateM nFuns genFun

measureFiveValues :: (f -> IO NominalDiffTime) -> Int -> [f] ->
  IO (NDT, NDT, NDT, NDT, NDT)
measureFiveValues timingFun nSamples funs = do
  sample <- sequenceA timingActions
  return $ fiveValueSummary sample
  where
    timingActions = map (medianMeasure timingFun nSamples) funs


fiveValueSummary :: (Ord a, Fractional a) => [a] -> (a, a, a, a, a)
fiveValueSummary sample = (min', median low, median', median high, max')
  where
    sample' = sort sample
    min' = head sample'
    max' = last sample'
    median' = median sample'
    (low, high) = splitSample sample'

splitSample :: [a] -> ([a], [a])
splitSample sample = (low, high')
  where
    ls = length sample
    mid = ls `div` 2
    (low, high) = splitAt mid sample
    high' = if odd ls then tail high else high
