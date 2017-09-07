-- | Module providing a Command Line Input API
module Proba.CLI(
  parse,
  run,
  help,
  Query(..), Result(..),
) where

import Proba.Core.Types
import Proba.Core.UniformEngine
import Proba.Core.Analytics
import Proba.Core.WeightedEngine
import Proba.Core.StatEngine

import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Map (toList)
-- | Query to Run from the parsed input, -- Computing the statistics for :
-- Either a weighted distribution
-- computed from the pillars for nSims and a selected uniform rng
-- can be specified as well.
-- Or a bounded uniform distribution given nSims and
-- a selected uniform rng
data Query =
      RunWeightedWith WPdfPillars Int UniformRNGType
    | RunUniformWith UPdfPillars Int UniformRNGType
  deriving (Eq)
instance NFData Query where
  rnf (RunWeightedWith (WPdfPillars p) n u) = rnf p `seq` rnf n
  rnf (RunUniformWith (UPdfPillars p) n u) = rnf n `seq` rnf p

instance Show Query where
  show (RunWeightedWith (WPdfPillars xs) n t)
    = "Stats on a Weighted Probabilities Run, with "
    ++ show n ++ " numbers, rng: " ++ show t
    ++ "\nInput PDF Pillars: \n" ++ unlines (map show xs)
  show (RunUniformWith (UPdfPillars xs) n t)
    = "Stats on a Uniform Probabilities Run, with "
    ++ show n ++ " numbers, rng: " ++ show t
    ++ "\nInput PDF Pillars: \n" ++ unlines (map show xs)

-- | Wrapper for the result type
data Result =
      ResultWeighted (CollectStats (Maybe Int), FinalStats (Maybe Int))
    | ResultUniform (CollectStats Double, FinalStats Double)

-- | The run type using either the weighted probability or
-- uniform probability engine
data RunType = Weighted | Uniform
  deriving (Show, Read)

-- | Helper to display the Results nicely
showRes :: Show a => String -> (CollectStats a, FinalStats a) -> String
showRes name (CollectStats dist n,
  FinalStats proba diffProba diffMean diffStd diffHi diffLow) = unlines $
     [
      "Result " ++ name ++ " Random Number Engine, " ++ show n ++ " random numbers."
     ,"Output PDF:"
     ] ++ showList' (toPillars proba)
     ++ ["", "Distribution:"]
     ++ showList' (toPillars dist)
     ++ ["", "Diff input and output PDFs:"]
     ++ showList' diffProba
     ++ ["", "Mean of the PDF Diffs; " ++ show diffMean]
     ++ ["StdDev of the PDF Diffs; " ++ show diffStd]
     ++ ["Highest of the PDF Diffs; " ++ show diffHi]
     ++ ["Lowest of the PDF Diffs; " ++ show diffLow]
  where
    showList' :: (Show a, Show b) => [(a, b)] -> [String]
    showList' = map (\(x, p) -> show x ++ ";" ++ show p )

instance Show Result where
  show (ResultWeighted stats) = showRes "Weighted" stats
  show (ResultUniform stats)  = showRes "Uniform" stats

-- | Parse the input argument into a query.
parse :: [String] -> Maybe Query
parse xs = do
    args <- getArgs
    let run     = fromMaybe Weighted $ fmap read $ lookup "run" args
        pillars = lookup "pillars" args
        nSims   = fromMaybe 1000000 $ fmap read $ lookup "nSims" args
        rng     = fromMaybe Mersenne $ fmap read $ lookup "rng" args
    return $ case run of
      Weighted -> RunWeightedWith
            (maybe pillarsWDefault WPdfPillars $ fmap read pillars) nSims rng
      Uniform -> RunUniformWith
            (maybe pillarsUDefault UPdfPillars $ fmap read pillars) nSims rng
  where
    getArgs :: Maybe [(String, String)]
    getArgs = do
      ys <- mapM getArgVal xs
      let args = ["pillars","nSims","rng", "run"]
      if all (==True) $ map (\ (x, _) -> x `elem` args) ys
         then Just ys
         else Nothing
    getArgVal :: String -> Maybe  (String, String)
    getArgVal (x:xs) = if x == '-'
         then Just $ let (n, v) = break (== '=') xs in (n, tail v)
        else Nothing

-- | Run a given query using the appropriate engine
run :: Query -> IO (Either String Result)
run (RunUniformWith pdfPillars nSims rngT) = do
  let p = mkUPEngineParams pdfPillars
  case p of
    Left s -> return $ Left s
    Right e -> do
      rng <- mkUniformRNG rngT
      return $ Right $ ResultUniform $
        computeStats e rng nSims
            (nextNum :: ProbaUniEngine Double)
run (RunWeightedWith pdfPillars nSims rngT) = do
  let p = mkWPEngineParams pdfPillars
  case p of
    Left s -> return $ Left s
    Right e -> do
      rng <- mkUniformRNG rngT
      return $ Right $ ResultWeighted $
        computeStats e rng nSims
          (nextNum :: ProbaWPEngine (Maybe Int))

-- | Give the CLI help
help :: [(String, String)]
help = [
    ("-help","Produce this help.")
   ,("-pillars='[(1,0.2),(2, 0.3),(3, 0.1),(4, 0.15)]'", "Input the PDF Pillars")
   ,("-nSims=1000000", "Number of random number generated")
   ,("-rng=Mersenne", "RNG type: Mersenne or Ecuyer")
   ,("-run=Weighted", "Run type: Weighted or Uniform")
  ]
