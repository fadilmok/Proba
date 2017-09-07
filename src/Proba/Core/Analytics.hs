{-# LANGUAGE TupleSections, RecordWildCards, FlexibleInstances, FlexibleContexts #-}
-- | Module containing simple analytics
module Proba.Core.Analytics(
  -- * Creation
  mkCdf, mkInvCdf,
  -- * Access
  invCdf,
  -- * Statistics
  mean, stdDev,
  statistics
) where

import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Proba.Core.Types

-- | Create a discrete cumulative function,
-- it assumes the pdf is correct
-- O(n)
mkCdf :: Ord a => PDF a -> CDF a
mkCdf = fromPillars . Map.foldlWithKey go [] . toRaw
  where
    go [] x p = [(x, p)]
    go (y:ys) x p = (x, p + snd y) : y : ys

-- | Create a discrete inverse cumulative function
-- it assumes the cdf is correct
-- O(n)
mkInvCdf :: CDF a -> InvCDF a
mkInvCdf = fromPillars . Prelude.map (\(x, y) -> (y, x)) . toPillars

-- | Inverse cdf function,
-- it assumes that the inputs are between 0 and 1 and that
-- the invCdf is correct, it fails otherwise.
-- O(log n)
invCdf :: InvCDF a -> Double -> a
invCdf curve x = snd $ curve !!! x

-- | Compute the statistics from the result distribution and original PDF
statistics :: (Ord a) => PDF a -> CollectStats a -> (CollectStats a, FinalStats a)
statistics inputPdf s@(CollectStats dist n)
  = (s, FinalStats {
      fsPDF     = resPdf
     ,fsDiffPDF = diffP
     ,fsDiffMean = mean diffs
     ,fsDiffStd = stdDev diffs
     ,fsDiffHi  = maximum diffs
     ,fsDiffLow = minimum diffs
     })
  where
    diffs = snd $ unzip diffP
    resPdf = fromRaw $ Map.map (\ x -> fromIntegral x / fromIntegral n) $ toRaw dist
    diffP = Map.toList $ Map.unionWith (-) (toRaw inputPdf) $ toRaw resPdf


-- | Compute the mean for a non empty list
-- O(n)
mean :: (Num a, Fractional a) => [a] -> a
mean [] = error "The input list is empty"
mean xs = sum xs / fromIntegral (length xs)

-- | Compute the standard deviation for a non empty list
-- O(n)
stdDev :: (Num a, Floating a, Fractional a) => [a] -> a
stdDev [] = error "The input list is empty"
stdDev xs = sqrt $
    ( sum $ map (\x -> (x - avg)^2) xs ) / fromIntegral ( length xs )
  where avg = mean xs
