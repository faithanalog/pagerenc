module Main (main) where

import Control.Monad.Free.Church
import Control.Monad.Trans.RWS.CPS
import qualified Data.ByteString.Builder as ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.FLEX as FLEX
import Data.Monoid
import Data.PCM
import qualified Data.POCSAG as POCSAG
import Data.Transmission
import Data.TransmissionParser
import Options.Applicative hiding (Failure, Parser, Success)
import qualified Options.Applicative as Opt
import System.IO
import System.Random
import qualified Text.Megaparsec as Megaparsec

-- multimon-ng's input sample rate
outRate :: SampleRate
outRate = SampleRate 22050

-- Amplitude for random noise between broadcasts
noiseAmplitude :: Double
noiseAmplitude = 0.3

data Options = Options
  { optThrottle :: Bool
  , optMinDelay :: Double
  , optMaxDelay :: Double
  }

runTransmission ::
     RandomGen g => Options -> TransmissionM ByteString a -> g -> ByteString
runTransmission opts tr g =
  ByteString.Builder.toLazyByteString . snd $ evalRWS (iterM run tr) () g
  where
    run (Transmit x m) = tell (ByteString.Builder.lazyByteString x) *> m
    run (Noise x f) = state (pcmNoise outRate noiseAmplitude x) >>= f
    run (RandDelayTime f) =
      state (randomR (optMinDelay opts, optMaxDelay opts)) >>= f
    run (Encode (Message t a m) f) =
      f $ pcmEncode outRate (messageBaudRate t) (enc a m)
      where
        enc =
          case t of
            FLEX -> FLEX.transmission
            POCSAG _ -> POCSAG.transmission


encodeTransmission :: Options -> IO ()
encodeTransmission opts = do
  input <- getContents
  case Megaparsec.parse transmission "" input of
    Left err -> hPutStrLn stderr $ Megaparsec.parseErrorPretty err
    Right x -> (runTransmission opts x <$> newStdGen) >>= write
  where
    write
      | optThrottle opts = throttledWrite outRate
      | otherwise = unthrottledWrite

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
    (long "mindelay" <>
     help "Set minimum delay between messages in seconds." <>
     metavar "NUM" <>
     value 1.0 <>
     showDefault) <*>
  option
    auto
    (long "maxdelay" <>
     help "Set maximum delay between messages in seconds." <>
     metavar "NUM" <>
     value 10.0 <>
     showDefault)
