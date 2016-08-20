{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.PCM
import Data.FLEX as FLEX
import Data.POCSAG as POCSAG
import Data.ByteString.Lazy (ByteString)
import System.IO
import System.Random
import Control.Monad
import Options.Applicative as Opt hiding (Failure, Success)

import Text.Trifecta as P hiding (option)
import Text.PrettyPrint.ANSI.Leijen (renderPretty, displayIO, linebreak)

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
message :: P.Parser Message
message = wait <|> msg
  where
    wait = do
        try $ text "WAIT"
        space <- optional $ oneOf " \t"
        case space of
            Nothing -> pure RandDelay
            Just s -> 
                whiteSpace *>
                (Delay . either fromIntegral id <$> naturalOrDouble)
    msg = do
        (tr,bd,delay) <- flex <|> pocsag
        char ':'
        address <- fromIntegral <$> decimal
        char ':'
        message <- many (noneOf "\r\n")
        pure $
            Transmission
                (pcmEncode outRate (BaudRate bd) (tr address message))
                delay
    flex = do
        text "FLEX"
        pure (FLEX.transmission, 1600, False)
    pocsag = do
        text "POCSAG"
        baud <- fromIntegral <$> decimal
        pure (POCSAG.transmission, baud, True)

messages :: P.Parser [Message]
messages = whiteSpace *> manyTill (message <* whiteSpace) eof

encodeMessages :: Options -> IO ()
encodeMessages opts = do
    input <- getContents
    let ms' :: Result [Message]
        ms' = parseString messages mempty input
    case ms' of
        Failure err -> displayIO stderr $ renderPretty 0.8 80 $ err <> linebreak
        Success ms -> mapM_ execMessage ms
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
