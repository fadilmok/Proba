{-# LANGUAGE ScopedTypeVariables #-}
-- | Testsuite for the Uniform Random Engine
module Test.Proba.Core.UniformEngine(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.Proba.QuickCheck as Test

import Control.Monad
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf (printf)

import Proba.Core.Types
import Proba.Core.UniformEngine
import Proba.Core.Analytics
import Proba.Core.StatEngine

-- | List of tests
tests :: TestSuite
tests = map (\(x, y) -> ("Uniform Engine - " ++ x, y))
  [
    ("Ecuyer Mean and Std",   propMeanStd Ecuyer)
   ,("Mersenne Mean And Std", propMeanStd Mersenne)
   ,("Ecuyer Bounds",         propBounds Ecuyer)
   ,("Mersenne Bounds",       propBounds Mersenne)
   ,("Mersenne Uniform",      propUniform Mersenne)
   ,("Ecuyer Uniform",        propUniform Ecuyer)
   ,("Mersenee Perf",         propPerf Mersenne)
   ,("Ecuyer Perf",           propPerf Ecuyer)
   ,("fail",                  testEngineFail)
  ]

defParams :: UEngineParams
defParams = UEngineParams $ fromPillars $ unUPP $ pillarsUDefault

-- | Test that the standard deviation and mean of the uniform
-- engine is constant.
propMeanStd :: UniformRNGType -> Test
propMeanStd rngT =
  TestQCRng rngT $ \ rng ->
    Test.runWith 10 $ \ (x :: Int) ->
      let vals = computeProba defParams rng
                    (nextNums nRand :: ProbaUniEngine [Double])
          m = mean vals
          std = stdDev vals
          x ~= y = abs (x - y) <= 0.04
       in m ~= 0.5 && std ~= 0.28

-- | Ensure that the uniform engine generate probilities within
-- the expected bounds
propBounds :: UniformRNGType -> Test
propBounds rngT =
  TestQCRng rngT $ \ rng ->
    Test.runWith 10 $ \ (x :: Int) ->
      foldl (\ acc x -> if not acc then False else  x >= 0 && x <= 1) True $
        computeProba defParams rng
          (nextNums nRand :: ProbaUniEngine [Double])

-- | Test the distribution of the uniform engine is uniform
propUniform :: UniformRNGType -> Test
propUniform rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ \ (x :: Int) ->
      let stats = computeStats defParams rng
                      1000000 (nextNum :: ProbaUniEngine Double)
          std = fsDiffStd $ snd stats
       in std < 0.001

-- | Ensure that the performance of the uniform engine
-- remain acceptable
propPerf :: UniformRNGType -> Test
propPerf rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ monadicIO $
  do
    t <- QC.run $ time $
      computeStats defParams rng
              100000 (nextNum :: ProbaUniEngine Double)

    let res = t < 0.45
    unless res $ do
      QC.run $ printf "Time %s: %0.9f sec" (show rT) t
    assert res

-- | Test that EngineParams fails to build for all
-- expected cases
testEngineFail :: Test
testEngineFail = TestPure $ const $
      negativePro && nullPro && greaterPro && nullPro2
  where
      negativePro = isLeft $ mkUPEngineParams $ UPdfPillars [(1, -1)]
      nullPro = isLeft $ mkUPEngineParams $
          UPdfPillars [(1,0.5), (2,0.3), (3,0.2)]
      nullPro2 = isLeft $ mkUPEngineParams $ UPdfPillars [(1,0)]
      greaterPro = isLeft $ mkUPEngineParams $
          UPdfPillars [(1, 0.4), (2, 0.5), (3, 0.4)]
