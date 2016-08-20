{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.PCM
import qualified Data.FLEX as FLEX
import qualified Data.POCSAG as POCSAG
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text.Lazy as Atto hiding (take, option)
import System.IO
import System.Random
import Control.Monad
import Options.Applicative as Opt

data Options = Options
    { optThrottle :: Bool
    , optMinDelay :: Double
    , optMaxDelay :: Double
    } 

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

-- multimon-ng's input sample rate
outRate = SampleRate 22050

-- Amplitude for random noise between broadcasts
noiseAmplitude = 0.3

data Message = Transmission ByteString Bool | Delay Double | RandDelay

-- Parse a message and generate a transmission from it
-- Messages can be either
-- FORMAT:ADDRESS:MESSAGE
-- WAIT
-- WAIT <LENGTH>
message :: Atto.Parser Message
message = wait <|> msg
  where
    wait = do
        "WAIT"
        space <- peekChar
        case space of
            Just ' ' -> Delay <$> double
            _ -> pure RandDelay
    msg = do
        (tr,bd,delay) <- flex <|> pocsag
        char ':'
        address <- decimal
        char ':'
        message <- T.unpack <$> takeTill isEndOfLine
        pure $
            Transmission
                (pcmEncode outRate (BaudRate bd) (tr address message))
                delay
    flex = "FLEX" *> pure (FLEX.transmission, 1600, False)
    pocsag = do
        "POCSAG"
        baud <- decimal
        pure (POCSAG.transmission, baud, True)

messages :: Atto.Parser [Message]
messages = skipSpace *> manyTill (message <* skipSpace) endOfInput

encodeMessages :: Options -> IO ()
encodeMessages opts = do
    input <- T.getContents
    either (hPutStrLn stderr) (mapM_ execMessage) (parseOnly messages input)
  where
    execMessage (Transmission bs delay) = 
        put bs >> when delay (execMessage RandDelay)
    execMessage (Delay delay) = noiseIO outRate noiseAmplitude delay >>= put
    execMessage RandDelay = 
        Delay <$> randomRIO (optMinDelay opts, optMaxDelay opts) >>=
        execMessage
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
             header
                 "pagerenc - a program for encoding FLEX and POCSAG messages")
    desc = 
        "Reads lines from stdin, outputting POCSAG or FLEX data to stdout\
        \ which is decodable by multimon-ng. Additionally, insert delays\
        \ between messages to simulate staggered broadcasts."
