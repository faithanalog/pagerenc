{-# LANGUAGE DeriveFunctor #-}
module Data.Transmission
  ( MessageType(..)
  , Message(..)
  , messageBaudRate
  , pcmEncodeMessage
  , POCSAGBaudRate(..)
  , pocsagBaudRate
  , Transmission(..)
  , TransmissionM
  , transmit
  , noise
  , randDelayTime
  , encode
  ) where

import Control.Monad.Free.Church
import Data.ByteString.Builder (Builder)
import qualified Data.FLEX as FLEX
import Data.PCM
import qualified Data.POCSAG as POCSAG
import Data.Word

data POCSAGBaudRate
  = POCSAGBaudRate512
  | POCSAGBaudRate1200
  | POCSAGBaudRate2400
  deriving (Eq, Read, Show)

pocsagBaudRate :: POCSAGBaudRate -> BaudRate
pocsagBaudRate POCSAGBaudRate512 = BaudRate 512
pocsagBaudRate POCSAGBaudRate1200 = BaudRate 1200
pocsagBaudRate POCSAGBaudRate2400 = BaudRate 2400

data MessageType
  = FLEX
  | POCSAG POCSAGBaudRate
  deriving (Eq, Read, Show)

messageBaudRate :: MessageType -> BaudRate
messageBaudRate (POCSAG r) = pocsagBaudRate r
messageBaudRate FLEX = BaudRate 1600

data Message = Message
  { msgType :: MessageType
  , msgAddr :: Word32
  , msgContents :: String
  } deriving (Eq, Read, Show)

pcmEncodeMessage :: SampleRate -> Message -> Builder
pcmEncodeMessage outRate (Message t a m) =
  pcmEncode outRate (messageBaudRate t) (enc t a m)
  where
    enc FLEX = FLEX.transmission
    enc (POCSAG _) = POCSAG.transmission

data Transmission w a =
  Transmit w a
  | Noise Double (w -> a)
  | RandDelayTime (Double -> a)
  | Encode Message (w -> a)
  deriving (Functor)

type TransmissionM w = F (Transmission w)

transmit :: w -> TransmissionM w ()
transmit msg = liftF $ Transmit msg ()

noise :: Double -> TransmissionM w w
noise x = liftF $ Noise x id

randDelayTime :: TransmissionM w Double
randDelayTime = liftF $ RandDelayTime id

encode :: Message -> TransmissionM w w
encode m = liftF $ Encode m id
