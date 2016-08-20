module Main where

import Data.List
import Data.PCM
import Data.FLEX as FLEX
import Data.POCSAG as POCSAG
import Data.ByteString.Lazy (ByteString)
import Data.Scientific
import System.IO
import System.Random
import Control.Monad
import Options.Applicative hiding (Failure, Success, Parser)
import qualified Options.Applicative as Opt

import Text.Megaparsec hiding (option, Message)
import Text.Megaparsec.String
import Text.Megaparsec.Lexer hiding (space)
import qualified Text.Megaparsec as P

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
message :: Parser Message
message = wait <|> msg
  where
    wait :: Parser Message
    wait = do
        try $ string "WAIT"
        many $ oneOf " \t"
        P.option RandDelay (Delay . toRealFloat <$> number)
    msg :: Parser Message
    msg = do
        (tr,bd,delay) <- flex <|> pocsag
        char ':'
        address <- fromIntegral <$> integer
        char ':'
        message <- many (noneOf "\r\n")
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
                [ string (show x) *> pure x
                | x <- [512, 1200, 2400] ]
        pure (POCSAG.transmission, baud, True)

messages :: Parser [Message]
messages = space *> manyTill (message <* space) eof

data Options = Options
    { optThrottle :: Bool
    , optMinDelay :: Double
    , optMaxDelay :: Double
    } 

encodeMessages :: Options -> IO ()
encodeMessages opts = do
    input <- getContents
    let ms' = parse messages "" input
    case ms' of
        Left err -> hPutStrLn stderr $ parseErrorPretty err
        Right ms -> mapM_ execMessage ms
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
