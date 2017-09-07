{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
-- | Module containing the Analytics unit tests
module Test.Proba.Core.Analytics(
  tests
) where

import Proba.Core.Analytics
import Proba.Core.WeightedEngine
import Proba.Core.Types

import Control.Exception
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck
import Test.Proba.QuickCheck

-- Instance to create correct PDFs
instance Arbitrary (PDF (Maybe Int)) where
  arbitrary = do
      pillars <- genPdfPillars
      let pdf = mkWPdf pillars
      return $ case pdf of
        Left s -> error $ "Generation failed " ++ s
        Right p -> p

-- Instance to create correct CDFs
instance Arbitrary (CDF (Maybe Int)) where
  arbitrary = mkCdf `liftM` arbitrary

-- Instance to create correct PDFs
instance Arbitrary (InvCDF (Maybe Int))  where
  arbitrary = mkInvCdf `liftM` arbitrary

-- Generatoring input for the inverseCDF testing
genInvCdfQuery :: Gen (PDF (Maybe Int), [Double])
genInvCdfQuery = do
  pdf <- arbitrary
  n <- choose (1, nTests)
  xs <- replicateM n $ choose(0, 1)
  return (pdf, xs)

-- Testsuite
tests :: TestSuite
tests = map (\(x, y) -> ("Analytics - " ++ x, y))
  [
    ("InvCDF Creation",                 propInvCdfFromPdfComplete)
   ,("CDF and PDF pillars check",       propCdfPdfPillars)
   ,("PDF stable with reverse Pillars", propPdfStable)
   ,("PDF consistency",                 propPdfConsistency)
   ,("InvCDF valid",                    propInvCdfValid)
   ,("PDF Failure",                     testPdfFail)
   ,("Inverse CDF Failure",             testInvCdfFail)
   ,("Mean Failure",                    testMeanFail)
   ,("Std Failure",                     testStdFail)
   ,("PDF No regression",               testPDFNoRegression)
   ,("InvCDF No regression",            testInvCDFNoRegression)
   ,("Statistics No regression",        testStatistics)
  ]

-- | Ensure that the last pillars of the CDF is 100%
propInvCdfFromPdfComplete :: Test
propInvCdfFromPdfComplete =
  TestQC $ run $ \ (pdf :: PDF (Maybe Int)) ->
    let iCdf = mkInvCdf $ mkCdf pdf
        lastP = fst $ last $ toPillars iCdf
     in abs ( lastP - 1) <= 0.0001

-- | Ensure that the PDF and CDF have the correct number of pillars
propCdfPdfPillars :: Test
propCdfPdfPillars =
  TestQC $ run $ \ (pdf :: PDF (Maybe Int)) ->
    let cdfPillars = fst $ unzip $ toPillars $ mkCdf pdf
        pdfPillars = fst $ unzip $ toPillars pdf
        invCdfPillars = snd $ unzip $ toPillars $ mkInvCdf $ mkCdf pdf
     in (length cdfPillars == length pdfPillars &&
            length pdfPillars == length invCdfPillars)
        &&
        (all (`elem` cdfPillars) pdfPillars &&
          all (`elem` pdfPillars) cdfPillars &&
            all (`elem` pdfPillars) invCdfPillars &&
              all (`elem` invCdfPillars) pdfPillars)

-- | Ensure that the pdf construction is stable
propPdfStable :: Test
propPdfStable =
  TestQC $ run $ forAll genPdfPillars $ \ (WPdfPillars pdfP) ->
    let (Right lhs) = mkWPdf (WPdfPillars pdfP)
        (Right rhs) = mkWPdf (WPdfPillars $ reverse $ pdfP )
    in lhs `eq` rhs

-- | Ensure that the pdf probabilities are correct.
propPdfConsistency :: Test
propPdfConsistency =
  TestQC $ run $ \ (pdf :: PDF (Maybe Int)) ->
    let s = cFoldl (\acc x -> x + acc) 0 pdf
     in abs (s - 1) <= 0.0001

-- | Ensure that the inverseCdf recovers the correct pdf pillars
propInvCdfValid :: Test
propInvCdfValid =
  TestQC $ run $ forAll genInvCdfQuery $ \(pdf :: PDF (Maybe Int), xs) ->
    let cdf = mkCdf pdf
        iCdf = mkInvCdf cdf
        set = Set.fromList $ fst $ unzip $ toPillars pdf
     in all (==True) $
           map (\ x -> invCdf iCdf x `Set.member` set) xs

-- | Ensure that the PDF construction fails when expected
testPdfFail :: Test
testPdfFail =
  TestPure $ const $
      negativePro && nullPro && greaterPro && nullPro2
  where
      negativePro = isLeft $ mkWPdf $ WPdfPillars [(1, -1)]
      nullPro = isLeft $ mkWPdf $ WPdfPillars []
      nullPro2 = isLeft $ mkWPdf $ WPdfPillars [(1,0)]
      greaterPro = isLeft $ mkWPdf $ WPdfPillars [(1, 0.4), (2, 0.5), (3, 0.4)]

-- | Ensure that inverse CDF fails when expected
testInvCdfFail :: Test
testInvCdfFail =
  TestIO $ failTest $
    let iCdf = fromPillars [(0.5, Just 1), (0.8, Just 2), (1, Just 3)]
    in invCdf iCdf 1.1

-- | Ensure that the mean fails when expected
testMeanFail :: Test
testMeanFail =
  TestIO $ failTest $ mean []

-- | Ensure that the stdDev fails when expected
testStdFail :: Test
testStdFail =
  TestIO $ failTest $ stdDev []

-- | Non regression test for the pdf
testPDFNoRegression :: Test
testPDFNoRegression =
  TestPure $ const $ test
    where
      pdfP = WPdfPillars [(1, 0.2), (1, 0.1), (2, 0.4), (3, 0.3),(4, 0)]
      res  = mkWPdf pdfP
      test = case res of
              Left _ -> False
              Right p ->
                p `eq` (fromPillars [(Just 1, 0.3), (Just 2, 0.4), (Just 3, 0.3)])

-- | Non regression test for the cdf
testInvCDFNoRegression :: Test
testInvCDFNoRegression =
  TestPure $ const test
    where
      pdfP = WPdfPillars [(1, 0.2), (1, 0.1), (2, 0.4), (3, 0.3),(4, 0)]
      res  = mkWPdf pdfP
      test = case res of
              Left _ -> False
              Right pdf ->
                mkInvCdf (mkCdf pdf) `eq'`
                  fromPillars [(0.3, Just 1), (0.7, Just 2), (1, Just 3)]

-- | Test the statistics
testStatistics:: Test
testStatistics =
  TestPure $ const $
    let
      dist = fromPillars [(Just 1, 10), (Just 2, 10), (Just 3, 80)]
        :: Distribution (Maybe Int)
      stats = CollectStats dist 100
      (CollectStats d n, FinalStats pb p m s h l) =
           statistics pdf stats
      pdf = fromPillars [(Just 1, 0.11), (Just 2, 0.1), (Just 3, 0.8)]
        :: PDF (Maybe Int)
      x ~= y = abs (x - y) < 0.001
      diff = fromPillars [(Just 1, 0.01), (Just 2, 0), (Just 3, 0)]
        :: PDF (Maybe Int)
   in m ~= (0.01/3) && h ~= 0.01 && l ~= 0 && s ~= 0.0057
        && pb `eq` pdf && fromPillars p `eq` diff

