-- | Testsuite for the Engine
module Test.Proba.Core.WeightedEngine(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.Proba.QuickCheck as Test

import Proba.Core.Types
import Proba.Core.WeightedEngine
import Proba.Core.StatEngine

import Control.Monad
import Data.Either
import Text.Printf (printf)

-- | List of tests related to the engine
tests :: TestSuite
tests = map (\(x, y) -> ("Weighted Engine - " ++ x, y))
  [
    ("distri check - Mersene", propWeightedProba Mersenne)
   ,("distri check - Ecuyer",  propWeightedProba Ecuyer)
   ,("fail",                   testEngineFail)
   ,("Perf - Mersene",         propPerf Mersenne)
   ,("Perf - Ecuyer",          propPerf Ecuyer)
  ]

-- | Test that we recover the input probabilities
-- using random pdf pillars as input
propWeightedProba :: UniformRNGType -> Test
propWeightedProba rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ forAll genPdfPillars $ \ pdfP ->
      let e = case mkWPEngineParams pdfP of
            Left s -> error s; Right x -> x
          stats = computeStats e rng 1000000
                    (nextNum :: ProbaWPEngine (Maybe Int))
          std = fsDiffStd $ snd stats
       in std < 0.001

-- | Test that EngineParams fails to build for all
-- expected cases
testEngineFail :: Test
testEngineFail = TestPure $ const $
      negativePro && nullPro && greaterPro && nullPro2
  where
      negativePro = isLeft $ mkWPEngineParams $ WPdfPillars [(1, -1)]
      nullPro = isLeft $ mkWPEngineParams $ WPdfPillars []
      nullPro2 = isLeft $ mkWPEngineParams $ WPdfPillars [(1,0)]
      greaterPro = isLeft $ mkWPEngineParams $
                      WPdfPillars [(1, 0.4), (2, 0.5), (3, 0.4)]

-- | Test that the performance of the Engine remain acceptable
propPerf :: UniformRNGType -> Test
propPerf rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ forAll genPdfPillars $ \ pdfP -> monadicIO $
    do
      let e'= mkWPEngineParams pdfP
          e = case e' of Left s -> error s; Right x -> x
      t <- QC.run $ time $
              computeStats e rng 100000
                    (nextNum :: ProbaWPEngine (Maybe Int))
      let res = t < 0.45
      unless res $
        QC.run $ printf "Time %s: %0.9f sec" (show rT) t
      assert res

