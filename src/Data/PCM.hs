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
import Data.BitsExtra
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import System.IO
import System.Random
import Data.Semigroup
import qualified Data.Resample


newtype SampleRate = SampleRate
  { getSampleRate :: Int
  }

newtype BaudRate = BaudRate
  { getBaudRate :: Int
  }

resamplePCM :: SampleRate -> SampleRate -> ByteString -> ByteString
resamplePCM (SampleRate src) (SampleRate dst) =
  B.toLazyByteString . Data.Resample.resample takeSample src dst
  where
    takeSample r f xs =
      maybe r (uncurry f) $ do
        (a, ta) <- B.uncons xs
        (b, tb) <- B.uncons ta
        pure $ (B.word8 a <> B.word8 b, tb)

-- Encode data as a series of 2-level PCM samples
pcmEncode :: SampleRate -> BaudRate -> ByteString -> ByteString
pcmEncode (SampleRate sr) (BaudRate br) =
  resamplePCM (SampleRate symRate) (SampleRate sr) . B.toLazyByteString .
  foldMap encodeWord . B.unpack
  where
    symRate = 38400
    sampleFor True = -maxBound
    sampleFor False = maxBound
    sampleRepeats = symRate `div` br
    encodeBit = stimesMonoid sampleRepeats . B.int16LE . sampleFor
    encodeWord = foldMap encodeBit . toBitsBE 8


-- Generate random pcmNoise. This is used instead of pure silence because
-- multimon-ng detects silence as if it was a signal, while it ignores pcmNoise
pcmNoise :: RandomGen g => SampleRate -> Double -> Double -> g -> (ByteString, g)
pcmNoise (SampleRate sr) amplitude duration =
  first B.toLazyByteString . samples
  where
    hi = floor $ amplitude * 32767
    lo = -hi
    len = round $ duration * fromIntegral sr :: Int
    samples g = gen (mempty, g)
    Endo gen =
      stimesMonoid len . Endo $ \x -> x >>= (first B.int16LE . randomR (lo, hi))

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
