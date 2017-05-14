module Main where

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.FLEX as FLEX
import Data.Monoid
import Data.PCM
import Data.POCSAG as POCSAG
import Data.Scientific
import Options.Applicative hiding (Failure, Parser, Success)
import qualified Options.Applicative as Opt
import System.IO
import System.Random
import Text.Megaparsec hiding (Message, option)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Lexer hiding (space)
import Text.Megaparsec.String

-- multimon-ng's input sample rate
outRate :: SampleRate
outRate = SampleRate 22050

-- Amplitude for random noise between broadcasts
noiseAmplitude :: Double
noiseAmplitude = 0.3

data Message
  = Transmission !ByteString
                 !Bool
  | Delay !Double
  | RandDelay

-- Parse a message and generate a transmission from it
-- Messages can be either
-- FORMAT:ADDRESS:MESSAGE
-- WAIT
-- WAIT <LENGTH>
message :: Parser Message
message = wait <|> msg
  where
    wait :: Parser Message
    wait = do
      string "WAIT"
      many $ oneOf " \t"
      P.option RandDelay $ Delay . toRealFloat <$> number
    msg :: Parser Message
    msg = do
      (tr, bd, delay) <- flex <|> pocsag
      char ':'
      address <- fromIntegral <$> integer
      char ':'
      message <- many $ noneOf "\r\n"
      pure $
        Transmission
          (pcmEncode outRate (BaudRate bd) (tr address message))
          delay
    flex = do
      string "FLEX"
      pure (FLEX.transmission, 1600, False)
    pocsag = do
      string "POCSAG"
      baud <-
        choice
          [ string "512" *> pure 512
          , string "1200" *> pure 1200
          , string "2400" *> pure 2400
          ]
      pure (POCSAG.transmission, baud, True)

messages :: Parser [Message]
messages = do
  space
  manyTill (message <* space) eof

data Options = Options
  { optThrottle :: Bool
  , optMinDelay :: Double
  , optMaxDelay :: Double
  }

encodeMessages :: Options -> IO ()
encodeMessages opts = do
  input <- getContents
  case parse messages "" input of
    Left err -> hPutStrLn stderr $ parseErrorPretty err
    Right msgs -> mapM_ execMessage msgs
  where
    execMessage (Transmission bs delay) = do
      put bs
      when delay $ execMessage RandDelay
    execMessage (Delay delayDuration) = do
      noise <- noiseIO outRate noiseAmplitude delayDuration
      put noise
    execMessage RandDelay = do
      delayDuration <- randomRIO (optMinDelay opts, optMaxDelay opts)
      execMessage $ Delay delayDuration
    -- Write samples to stdout
    put
      | optThrottle opts = throttledPut outRate
      | otherwise = unthrottledPut

main :: IO ()
main = execParser opts >>= encodeMessages
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
