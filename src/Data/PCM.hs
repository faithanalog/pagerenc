module Data.PCM
  ( pcmEncode
  , pcmNoise
  , throttledPutStr
  , writeSamples
  , SampleRate(..)
  , BaudRate(..)
  ) where

import Control.Concurrent
import Data.Bifunctor
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import System.IO
import System.Random
import Data.Semigroup
import qualified Data.Resample
import Control.Monad


newtype SampleRate = SampleRate
  { getSampleRate :: Int
  }

newtype BaudRate = BaudRate
  { getBaudRate :: Int
  }

-- Encode bits as a series of 2-level PCM samples
pcmEncode :: SampleRate -> BaudRate -> [Bool] -> Builder
pcmEncode (SampleRate sr) (BaudRate br) =
  Data.Resample.resample uncons br sr
  where
    sampleFor True = -maxBound
    sampleFor False = maxBound
    encodeBit = B.int16LE . sampleFor
    uncons [] _ = mempty
    uncons (x:xs) f = f (encodeBit x) xs

-- Generate random pcmNoise. This is used instead of pure silence because
-- multimon-ng detects silence as if it was a signal, while it ignores pcmNoise
pcmNoise :: RandomGen g => SampleRate -> Double -> Double -> g -> (Builder, g)
pcmNoise (SampleRate sr) amplitude duration = noise B.int16LE (lo, hi) len
  where
    hi = floor $ amplitude * 32767
    lo = -hi
    len = round $ duration * fromIntegral sr

-- | Generate n values within a range, combining them with a function mapping
-- to a monoid. This makes use of the Monad instance for (,)
noise :: (RandomGen g, Random a, Monoid m) => (a -> m) -> (a, a) -> Int -> g -> (m, g)
noise m range n g =
  flip appEndo (mempty, g) . stimesMonoid n $
  Endo (>>= (first m . randomR range))

-- Limits the rate at which samples are calculated to yield data as if it was
-- being FM decoded in real time. This does technically output slightly slower
-- than it should, because write-time is not accounted for when calculating
-- sleep delays.
throttledPutStr :: SampleRate -> ByteString -> IO ()
throttledPutStr (SampleRate sr) samples = do
  B.putStr samples
  hFlush stdout
  threadDelay sleepTime
  where
    numSamples = fromIntegral (B.length samples `div` 2) -- 2 bytes per sample
    sleepTime = 1000000 * numSamples `div` sr -- (1000000 us / sec) * (1 sec / sr smpls) * (numSamples smpls / chunk)

-- | Write chunks using a caller-supplied write function, allowing the caller
-- to insert delays, etc.
writeSamples :: Applicative f => (ByteString -> f ()) -> ByteString -> f ()
writeSamples write samples =
  unless (B.null samples) $ write h *> writeSamples write t
  where
    chunkSize = 4096
    (h, t) = B.splitAt chunkSize samples
    
