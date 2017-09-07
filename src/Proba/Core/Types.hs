{-# LANGUAGE BangPatterns, FlexibleInstances,
          GeneralizedNewtypeDeriving, MultiParamTypeClasses,
          FunctionalDependencies, FlexibleContexts
          #-}
-- | Module containing all the key Types
module Proba.Core.Types(
  -- * Engines
  ProbaEngine(..),
  ProbaUniEngine(..),
  ProbaWPEngine(..),
  StatEngine,
  -- * Types
  WPdfPillars(..), UPdfPillars(..),
  CDF(..), PDF(..), InvCDF(..),
  Distribution(..),
  UniformRNGType(..),
  UniformRNG(..),
  WEngineParams(..), UEngineParams(..),
  CollectStats(..),
  FinalStats(..),
  Curve(..),
  PieceWiseCurve(..)
) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Map.Strict as Map
import System.Random as Ecuyer
import qualified System.Random.Mersenne.Pure64 as Mersenne

-- | Uniform Random Number generator,
-- containing Ecuyer and Mersenne Twister rng
data UniformRNG
  = RandomEcuyer   {-# UNPACK #-} !Ecuyer.StdGen
  | RandomMersenne {-# UNPACK #-} !Mersenne.PureMT
  deriving Show

-- | Enum type allowing the selection of Uniform RNG
data UniformRNGType = Ecuyer | Mersenne
  deriving (Show, Read, Eq)

-- | PDF Pillars
newtype WPdfPillars = WPdfPillars { unWPP ::  [(Int, Double)] }
  deriving (Show, Eq)
newtype UPdfPillars = UPdfPillars { unUPP :: [(Double, Double)] }
  deriving (Show, Eq)

-- | Discrete PieceWise Curve modelised using a TreeMap
newtype PieceWiseCurve a b = PieceWiseCurve { unPWC :: Map a b }
  deriving (Show )
-- | Discrete Probabily Density Function,
newtype PDF a = PDF { unPDF :: PieceWiseCurve a Double }
  deriving (Show, Curve a Double, NFData)
-- | Discrete Cumulative Distribution Function
newtype CDF a = CDF { unCDF :: PieceWiseCurve a Double}
  deriving (Show, Curve a Double)
-- | Discrete Inverse Cumulative Distribution Function
newtype InvCDF a = InvCDF { unICDF :: PieceWiseCurve Double a }
  deriving (Show, Curve Double a)
-- | Distribution
newtype Distribution a = Distribution { unDist :: PieceWiseCurve a Int }
  deriving (Show, Curve a Int, NFData)

-- | Operations on a Curve.
class Curve b c a | a -> b, a -> c where
  emptyCurve :: a
  -- | O(1) Nb of Pillars
  nPillars :: a -> Int
  -- | O(n log n) create a curve from pillars
  fromPillars :: Ord b => [(b, c)] -> a
  -- | O(n) return the pillars from a curve
  toPillars :: a -> [(b, c)]
  -- | O(1) create a curve from a map
  fromRaw :: Map b c -> a
  -- | O(1) get the inner map
  toRaw :: a -> Map b c
  -- | O(log n) get the values associated to a key
  -- it is unsafe throws an exception if it is out of bounds.
  (!!!) :: (Show b, Ord b) => a -> b -> (b, c)
  -- | O(log n) add a pillar with a combining function
  addWith :: Ord b => (c -> c -> c) -> b -> c -> a -> a
  -- | O(log n) add a pillar
  add :: Ord b => b -> c -> a -> a
  -- | O(n) add a pillar with a combining function
  cFoldl :: (d -> c -> d) -> d -> a -> d
  -- | O(n) appy a function to each pillar
  cMap :: (c -> c) -> a -> a

-- | Weighted Probability Engine Params
-- contains the PDF, CDF, inverseCDF
-- needed to compute the weighted probabilities
data WEngineParams =
  WEngineParams {
    pdf     :: PDF (Maybe Int)
   ,cdf     :: CDF (Maybe Int)
   ,iCdf    :: !(InvCDF (Maybe Int))
  }

-- | Uniform Probability Engine Params
-- Used to pick the buckets, the probabilities are flat
newtype UEngineParams =
  UEngineParams {
    uepPdf  :: PDF Double
  }

-- | Statistic we collect during the Run [a]
--  csDistri  : represent the distribution of the random numbers
--  csCount   : the total nb of element
data CollectStats a =
  CollectStats {
    csDistri      :: !(Distribution a)
   ,csCount       :: !Int
  }

-- | Statistics we compute after the run
--  fsPDF     : the computed PDF from the distribution
--  fsDiffPDF : the difference between the original and computed PDF
--  fsDiffMean: the mean of the differences between PDFs
--  fsDiffStd : the standard deviation of the difference between PDFs
--  fsDiffHi  : the highest differences
--  fsDiffLow : the lowest differences
data FinalStats a =
  FinalStats {
    fsPDF         :: PDF a
   ,fsDiffPDF     :: [(a, Double)]
   ,fsDiffMean    :: Double
   ,fsDiffStd     :: Double
   ,fsDiffHi      :: Double
   ,fsDiffLow     :: Double
  }

-- | Class to compute probabilities given an engine.
class Monad a => ProbaEngine a b c | a -> b, a -> c where
  -- | get the PDF from the params
  getPDF :: a b -> c -> PDF b
  -- | Compute the probabilities
  computeProba :: c -> UniformRNG -> a d -> d
  -- | Compute the probabilities and send back the rng
  runProba :: c -> UniformRNG -> a d -> (d, UniformRNG)
  -- | Prepare the next random number
  nextNum :: a b
  -- | Prepare the n next random numbers
  nextNums :: Int -> a [b]
  nextNums n = replicateM n nextNum

-- | Bounded Uniform Probability Engine
newtype ProbaUniEngine a = ProbaUniEngine { unPUIE :: State UniformRNG a }
  deriving (Functor, Applicative, Monad, MonadState UniformRNG)

-- | Weighted Probility Engine
newtype ProbaWPEngine a = ProbaWPEngine {
     unPWPE :: ReaderT WEngineParams (State UniformRNG) a
   } deriving (
      Functor, Applicative, Monad, MonadState UniformRNG, MonadReader WEngineParams)

type StatEngine a b c =
   ReaderT (a b, c) (State (CollectStats b, UniformRNG)) (CollectStats b)

-- Instances

instance Functor (PieceWiseCurve a) where
  fmap f (PieceWiseCurve m) = PieceWiseCurve $ Map.map f m

instance Curve a b (PieceWiseCurve a b) where
  emptyCurve = PieceWiseCurve Map.empty
  nPillars    = Map.size . toRaw
  fromPillars = fromRaw . Map.fromList
  toPillars   = Map.toList . toRaw
  fromRaw     = PieceWiseCurve
  toRaw       = unPWC
  PieceWiseCurve m !!! x =
    case x `Map.lookupGE` m of
      Just r -> r
      Nothing -> error $ "Fails: " ++ show x
  addWith f x y c = PieceWiseCurve $ Map.insertWith f x y $ toRaw c
  add x y c       = PieceWiseCurve $ Map.insert x y $ toRaw c
  cFoldl f x m    = Map.foldl f x $ toRaw m
  cMap = fmap

instance NFData a => NFData (CollectStats a) where
  rnf (CollectStats d t) = rnf d `seq` rnf t

instance NFData a => NFData (FinalStats a) where
  rnf (FinalStats p dP dM dS dH dL) = rnf p
        `seq` rnf dP `seq` rnf dM `seq` rnf dS `seq` rnf dH `seq` rnf dL

instance (NFData a, NFData b) => NFData (PieceWiseCurve a b) where
  rnf (PieceWiseCurve c) = rnf c
