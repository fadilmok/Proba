{-# OPTIONS_GHC -O2 #-}
-- | Module to run all the testsuites
module Test.Proba.Run (
  main
) where

import Control.Monad
import System.Exit (exitFailure)
import Text.Printf
import Test.QuickCheck

import Test.Proba.QuickCheck (runTest)
import qualified Test.Proba.Core.Analytics as Analytics
import qualified Test.Proba.Core.UniformEngine as UniformEngine
import qualified Test.Proba.Core.WeightedEngine as WeightedEngine
import qualified Test.Proba.CLI as CLI

-- | Main to run the testsuite, does not stop for failure
main :: IO()
main = do
  success <- forM (
      CLI.tests ++
      Analytics.tests ++
      UniformEngine.tests ++
      WeightedEngine.tests
   ) $ \ (name, test) -> do
           printf "%-45s:" name
           runTest test

  unless (all (==True) success)
    exitFailure

