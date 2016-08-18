module Data.PCM
  (pcmEncode
  ,noise
  ,noiseIO
  ,throttledPut
  ,unthrottledPut
  ,SampleRate(..)
  ,BaudRate(..))
  where

import Data.BitsExtra
import Data.Chunks
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (Builder)
import System.IO
import System.Random
import Control.Concurrent
import Control.Monad

newtype SampleRate = SampleRate Int
newtype BaudRate = BaudRate Int

-- Resample using a nearest-neighbor algorithm
resample :: SampleRate -> SampleRate -> [a] -> [a]
resample (SampleRate src) (SampleRate dst) = go 0 0
  where
    indexFor o = o * src `div` dst
    go _ _ [] = []
    go i o xs
      | indexFor o > i = go (i + 1) o (tail xs)
      | otherwise = head xs : go i (o + 1) xs
    

-- Encode data as a series of 2-level PCM samples
pcmEncode :: SampleRate -> BaudRate -> ByteString -> ByteString
pcmEncode (SampleRate sr) (BaudRate br) = 
    B.toLazyByteString .
    mconcat .
    resample (SampleRate symRate) (SampleRate sr) .
    concatMap encodeWord . B.unpack
  where
    symRate = 38400
    sampleFor True = -maxBound
    sampleFor False = maxBound
    sampleRepeats = symRate `div` br
    encodeBit = replicate sampleRepeats . B.int16LE . sampleFor
    encodeWord = concatMap encodeBit . toBitsBE 8


-- Generate random noise. This is used instead of pure silence because
-- multimon-ng detects silence as if it was a signal, while it ignores noise
noise :: RandomGen g => SampleRate -> Double -> Double -> g -> ByteString
noise (SampleRate sr) amplitude duration = 
    B.toLazyByteString . mconcat . map B.int16LE . samples
  where
    hi = floor (amplitude * 32767)
    lo = -hi
    len = round (duration * fromIntegral sr)
    samples = take len . randomRs (lo, hi)

noiseIO :: SampleRate -> Double -> Double -> IO ByteString
noiseIO sr a d = noise sr a d <$> newStdGen

-- Limits the rate at which samples are calculated to yield data as if it was
-- being FM decoded in real time.
throttledPut :: SampleRate -> ByteString -> IO ()
throttledPut (SampleRate sr) samples = do
    let chunkSize = 4096
        -- (1000000 us / sec) * (1 sec / sr smpls) * (chunkSize smpls / chunk)
        sleepTime = 1000000 * chunkSize `div` sr
        chunks = chunksOfB (fromIntegral chunkSize) samples
    forM_ chunks $ \xs -> do
        unthrottledPut xs
        hFlush stdout
        threadDelay sleepTime

-- Output samples as fast as we can
unthrottledPut :: ByteString -> IO ()
unthrottledPut = B.putStr
