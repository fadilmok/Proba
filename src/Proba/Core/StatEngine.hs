{-# LANGUAGE BangPatterns #-}
module Proba.Core.StatEngine (
  computeStats
) where

import Proba.Core.Types
import Proba.Core.Analytics

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map as Map

computeStats :: (Ord b, Show b, ProbaEngine a b c) =>
  c -> UniformRNG -> Int -> a b -> (CollectStats b, FinalStats b)
computeStats params rng n engine =
  let pdf = getPDF engine params
      d = mkInitialDistri pdf
   in statistics pdf $
        flip evalState (CollectStats d 0, rng) $
          flip runReaderT (engine, params) $
            mkStats n

mkStats :: (Ord b, Show b, ProbaEngine a b c) => Int -> StatEngine a b c
mkStats 0 = error "Please query at least one stat"
mkStats 1 = do
   (!engine, !params) <- ask
   (!stats, !rng) <- get
   let (!y, !rng') = runProba params rng engine
       !stats' = stats {
                 csDistri = addWith (+) (fst $ (csDistri stats) !!! y) 1 $
                     csDistri stats
                ,csCount = csCount stats + 1
              }
   put (stats', rng')
   return stats'
mkStats n = allStats' (n - 1) $ mkStats 1
  where
     allStats' 0 acc = acc
     allStats' !n acc = allStats' (n - 1) $ acc >> mkStats 1

mkInitialDistri :: PDF a -> Distribution a
mkInitialDistri = fromRaw . Map.map (const 0 :: Double -> Int) . toRaw
