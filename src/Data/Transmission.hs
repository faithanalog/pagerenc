{-# LANGUAGE DeriveFunctor #-}
module Data.Transmission
  ( MessageType(..)
  , Message(..)
  , messageBaudRate
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
import Data.Word
import Data.PCM

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
