module Data.PCM
  ( pcmEncode
  , pcmNoise
  , throttledWrite
  , unthrottledWrite
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
import qualified Data.List as List


newtype SampleRate = SampleRate
  { getSampleRate :: Int
  }

newtype BaudRate = BaudRate
  { getBaudRate :: Int
  }

-- Encode bits as a series of 2-level PCM samples
pcmEncode :: Foldable t => SampleRate -> BaudRate -> t Bool -> Builder
pcmEncode (SampleRate sr) (BaudRate br) =
  Data.Resample.resample uncons symRate sr . foldMap encodeBit
  where
    symRate = 38400
    sampleFor True = -maxBound
    sampleFor False = maxBound
    sampleRepeats = symRate `div` br
    encodeBit = replicate sampleRepeats . B.int16LE . sampleFor
    uncons r f xs = maybe r (uncurry f) $ List.uncons xs

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
throttledWrite :: SampleRate -> ByteString -> IO ()
throttledWrite (SampleRate sr) = writeSamples
  where
    chunkSize = 4096
    chunkSizeBytes = chunkSize * 2 -- 2 bytes per sample
    -- (1000000 us / sec) * (1 sec / sr smpls) * (chunkSize smpls / chunk)
    sleepTime = 1000000 * chunkSize `div` sr
    writeSamples samples
      | B.null samples = pure ()
      | otherwise = do
        unthrottledWrite h
        hFlush stdout
        threadDelay sleepTime
        writeSamples t
      where
        (h, t) = B.splitAt (fromIntegral chunkSizeBytes) samples

-- Output samples as fast as we can
unthrottledWrite :: ByteString -> IO ()
unthrottledWrite = B.putStr
