module Main where

import Data.List
import Data.PCM
import qualified Data.FLEX as FLEX
import qualified Data.POCSAG as POCSAG
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import System.IO
import System.Random
import Control.Monad
import Options.Applicative

data Options = Options
    { optThrottle :: Bool
    , optMinDelay :: Double
    , optMaxDelay :: Double
    } 

defaultOptions = 
    Options
    { optThrottle = False
    , optMinDelay = 1
    , optMaxDelay = 10
    }
options :: Parser Options
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

readMaybe :: Read a => String -> Maybe a
readMaybe str =
    case reads str of
        [(x,_)] -> Just x
        _ -> Nothing

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither str = maybe (Left str) Right

-- Parse a message and generate a transmission from it
-- Messages can be either
-- FORMAT:ADDRESS:MESSAGE
-- WAIT
-- WAIT <LENGTH>
--
-- Incoming ugly code :(
parseMessage :: String -> Either String Message
parseMessage "WAIT" = pure RandDelay
parseMessage ('W':'A':'I':'T':' ':ln) = 
    Delay <$> maybeToEither ("Invalid delay: \"" ++ ln ++ "\"") (readMaybe ln)
parseMessage ln = do
    (fmt,addr,msg) <- maybeToEither ("Malformed line: \"" ++ ln ++ "\"") fields
    (tr,bd,delay) <- 
        case fmt of
            "FLEX" -> Right (FLEX.transmission, 1600, False)
            "POCSAG512" -> Right (POCSAG.transmission, 512, True)
            "POCSAG1200" -> Right (POCSAG.transmission, 1200, True)
            "POCSAG2400" -> Right (POCSAG.transmission, 2400, True)
            _ -> Left $ "Invalid format: \"" ++ fmt ++ "\""
    addrWord <- 
        maybeToEither ("Invalid address: \"" ++ addr ++ "\"") (readMaybe addr)
    pure $
        Transmission (pcmEncode outRate (BaudRate bd) $ tr addrWord msg) delay
  where
    split xs = do
        c <- elemIndex ':' xs
        Just (take c xs, drop (c + 1) xs)
    fields = do
        (fmt,ln') <- split ln
        (addr,msg) <- split ln'
        Just (fmt, addr, msg)

encodeMessages :: Options -> IO ()
encodeMessages opts = do
    input <- getContents
    forM_
        [ parseMessage x
        | x <- lines input 
        , not (null x) ] $
        either
            (\err -> 
                  hPutStrLn stderr err)
            execMessage
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
