module Main (main) where

import Control.Monad.Free.Church
import Control.Monad.Trans.RWS.CPS
import qualified Data.ByteString.Builder as ByteString.Builder
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as ByteString
import Data.Monoid
import Data.PCM
import Data.Transmission
import Data.TransmissionParser
import Options.Applicative hiding (Failure, Parser, Success)
import qualified Options.Applicative as Opt
import System.IO
import System.Random
import qualified Text.Megaparsec as Megaparsec

data Options = Options
  { optThrottle :: Bool
  , optMinDelay :: Double
  , optMaxDelay :: Double
  , optNoiseVolume :: Double
  , optSampleRate :: SampleRate
  }

runTransmission ::
     RandomGen g => Options -> TransmissionM Builder a -> g -> Builder
runTransmission opts tr g = snd $ evalRWS (iterM run tr) () g
  where
    run (Transmit x f) = tell x *> f
    run (Noise x f) =
      state (pcmNoise (optSampleRate opts) (optNoiseVolume opts) x) >>= f
    run (RandDelayTime f) =
      state (randomR (optMinDelay opts, optMaxDelay opts)) >>= f
    run (Encode x f) = f $ pcmEncodeMessage (optSampleRate opts) x

encodeTransmission :: Options -> IO ()
encodeTransmission opts = do
  input <- getContents
  case Megaparsec.parse transmission "" input of
    Left err -> hPutStrLn stderr $ Megaparsec.parseErrorPretty err
    Right x ->
      (runTransmission opts x <$> newStdGen) >>=
      (write . ByteString.Builder.toLazyByteString)
  where
    write
      | optThrottle opts = writeSamples (throttledPutStr (optSampleRate opts))
      | otherwise = writeSamples ByteString.putStr

main :: IO ()
main = execParser opts >>= encodeTransmission
  where
    opts =
      info
        (helper <*> options)
        (fullDesc <> progDesc desc <>
         header "pagerenc - a program for encoding FLEX and POCSAG messages")
    desc =
      "Reads lines from stdin, outputting POCSAG or FLEX data to stdout\
        \ which is decodable by multimon-ng. Additionally, insert delays\
        \ between messages to simulate staggered broadcasts."

options :: Opt.Parser Options
options =
  Options <$>
  switch
    (long "throttle" <>
     help "Throttle data output to 22050Hz, causing 'realtime' playback.") <*>
  option
    auto
    (long "mindelay" <> help "Set minimum delay between messages in seconds." <>
     metavar "NUM" <>
     value 1.0 <>
     showDefault) <*>
  option
    auto
    (long "maxdelay" <> help "Set maximum delay between messages in seconds." <>
     metavar "NUM" <>
     value 10.0 <>
     showDefault) <*>
  option
    auto
    (long "noisevolume" <> help "Set volume of noise inserted between messages." <>
     metavar "NUM" <>
     value 0.3 <>
     showDefault) <*>
  option
    (fmap SampleRate auto)
    (long "samplerate" <> help "Set sample rate of output data." <>
     metavar "INT" <>
     value (SampleRate 22050) <>
     showDefaultWith (\(SampleRate x) -> show x))
