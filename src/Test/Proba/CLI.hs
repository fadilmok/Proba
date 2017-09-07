-- | Module testing the Command line input
module Test.Proba.CLI(
  tests
) where

import Proba.CLI
import Proba.Core.Types
import Proba.Core.WeightedEngine
import Proba.Core.UniformEngine
import Test.Proba.QuickCheck hiding (run)

import Control.Monad

-- | Testsuite
tests :: TestSuite
tests = [
    ("CLI - Query", testQuery)
   ,("CLI - Run",   testRun)
  ]

-- | Test the creation of a query from the arguments
testQuery :: Test
testQuery = TestPure $ const $ t1 && t2 && t3 && t4 && t5
  where
      t1 = parse [] == Just (RunWeightedWith pillarsWDefault 1000000 Mersenne)
      t2 = parse ["-rng=Ecuyer"] ==
                Just (RunWeightedWith pillarsWDefault 1000000 Ecuyer)
      t3 = parse ["-rng=Ecuyer","-nSims=1"] ==
                Just (RunWeightedWith pillarsWDefault 1 Ecuyer)
      t4 = parse ["-rng=Ecuyer","-nSims=1","-pillars=[(1,0),(2,1)]"] ==
                Just (RunWeightedWith (WPdfPillars [(1, 0), (2, 1)]) 1 Ecuyer)
      t5 = parse ["-rng=Ecuyer","-nSims=1","-run=Uniform"] ==
                Just (RunUniformWith pillarsUDefault 1 Ecuyer)

-- | Test a run for a given query
testRun :: Test
testRun = TestIO $ do
  let q1 = RunWeightedWith (WPdfPillars [(1, 0.5), (2, 0.5)]) 1000000 Mersenne
      q2 = RunUniformWith pillarsUDefault 1000000 Mersenne

  r1' <- run q1
  let r1 = either (const False) (
         \ (ResultWeighted (_, FinalStats  r _ _ _ _ _)) ->
            foldl (\ acc (_, x) ->
              if not acc then False
                      else round (x * 100) == 50) True $ toPillars r
            ) r1'

  r2' <- run q2
  let r2 = either (const False) (
          \ (ResultUniform (_,  FinalStats r _ _ _ _ _)) ->
            foldl (\ acc (_, x) ->
              if not acc then False
                else round (x * 100) ==
                  round (100 / fromIntegral (nPillars r))) True $ toPillars r
          ) r2'
  let res = r1 && r2
  unless res $ do
    print $ "RunWeighted: " ++ show r1'
    print $ "RunUniform: " ++ show r2'

  return res
