{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances #-}
-- | The weighted proba engine
module Proba.Core.WeightedEngine(
  -- * Creation
  mkWPEngineParams, mkWPdf,
  pillarsWDefault,
  -- * Engines
  ProbaWPEngine()
) where

import Proba.Core.Types
import Proba.Core.UniformEngine
import Proba.Core.Analytics

import Data.List (foldl')
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map as Map

-- | Default pillars if not specified
pillarsWDefault :: WPdfPillars
pillarsWDefault = WPdfPillars [(1, 0.2), (2, 0.3), (3, 0.1), (4, 0.15)]

-- | Creates discrete probability function.
-- The pillars cannot be null, contain negative probability,
-- the sum of the probability cannot exceed 1.
-- The pillars with probability 0 are discarded.
-- O(n log n)
mkWPdf :: WPdfPillars -> Either String (PDF (Maybe Int))
mkWPdf (WPdfPillars xs)
  | null xs = Left "The pdf pillars are empty."
  | null $ filter (\(_, x) -> x /= 0) xs =
      Left "The pdf pillars contain only zero."
  | foldl (\ acc (_, x) -> if acc then acc else x < 0 || x > 1) False xs =
      Left "PDF Pillars contain negative values or greater than 1."
  | foldl (\ acc (_, x) -> acc + x) 0 xs > 1 =
      Left "The sum of PDF probabilities are greater than 1."
  | otherwise = Right $
      let m = foldl (\ m (v, p) ->
            if p == 0 then m else addWith (+) (Just v) p m) emptyCurve xs
          s = foldl (\acc (_, x)-> acc + x) 0 xs
       in (if s < 1 then add Nothing (1 - s) else id) m

-- | Create a weighted probability engine parameters
-- The creation can fail if the pdf pillars are incorrect.
--
mkWPEngineParams :: WPdfPillars -> Either String WEngineParams
mkWPEngineParams pdfP =
  either
      (\ msg -> Left msg)
      (\ p -> let cdf' = mkCdf p in
        Right WEngineParams {
            pdf = p
           ,cdf = cdf'
           ,iCdf = mkInvCdf cdf'
         }) $ mkWPdf pdfP

instance ProbaEngine ProbaWPEngine (Maybe Int) WEngineParams where

  computeProba p uniRng e =
    flip evalState uniRng $
      runReaderT (unPWPE e) p

  runProba p r e =
    let (!x, !rng) = flip runState r $
                      runReaderT (unPWPE e) p
    in (x, rng)

  nextNum = do
    WEngineParams _ _ iCdf <- ask
    uniRng <- get
    let (!x, !r) = runState (unPUIE nextNum) uniRng
    put r
    let !y = invCdf iCdf x
    return y

  getPDF _ p = pdf p

