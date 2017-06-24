module Data.POCSAG
  (transmission)
  where

import Data.Bits
import Data.BitsExtra
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy (ByteString)
import Data.CRC
import Data.List.Split
import Data.Word

-- Fully encoded codeword with CRC added
newtype Codeword = Codeword { getCodeword :: Word32 }

-- POCSAG constants

-- POCSAG SYNC codeword
sync :: Codeword
sync = Codeword 0x7CD215D8

-- POCSAG IDLE codeword
idle :: Codeword
idle = Codeword 0x7A89C197

-- POCSAG Preamble of 576 bits, repeating 1,0,1,0 .. 
preamble :: [Codeword]
preamble = replicate (div 576 32) (Codeword 0xAAAAAAAA)

-- First bit of a message codeword is 1
msgFlag :: Word32
msgFlag = 0x100000

-- First bit of an address codeword is 0
addrFlag :: Word32
addrFlag = 0x000000

-- 0x3 in the low two bits of a POCSAG address indicates text data
textFlag :: Word32
textFlag = 0x3

-- 0x0 in the low two bits of a POCSAG address indicates numeric data
numericFlag :: Word32
numericFlag = 0x0

-- Number of data bits per codeword
msgSize :: Int
msgSize = 20

-- Number of words in a frame
frameSize :: Int
frameSize = 2

-- Number of words in a batch
batchSize :: Int
batchSize = 16


codewordBE :: Codeword -> Builder
codewordBE (Codeword x) = B.word32BE x

-- Takes the 21 bit data segment of a codeword and adds the CRC and parity
-- error correction codes
codeword :: Word32 -> Codeword
codeword x = Codeword $ shiftL fullCRC 1 .|. parityBit fullCRC
  where
    fullCRC = shiftL x crcLength .|. crc x

-- Encode an ASCII message to a series of codewords
encodeASCII :: String -> [Codeword]
encodeASCII = map mkCodeword . chunksOf msgSize . concatMap charToBits
  where
    charToBits = toBitsLE 7 . (fromIntegral :: Int -> Word8) . fromEnum
    mkCodeword bs = codeword $ msgFlag .|. fromBitsBE msgSize bs

transmission :: Word32 -> String -> ByteString
transmission addr txt = toBytes $ preamble ++ insertSyncs trData
  where
    addrFrameNum = fromIntegral $ addr .&. 0x7
    addrWord = codeword $ shiftL (shiftR addr 3) 2 .|. textFlag .|. addrFlag
    trData =
      concat
        [ replicate (addrFrameNum * frameSize) idle
        , [addrWord]
        , encodeASCII txt
        , [idle]
        ]
    -- Insert syncs between batches
    insertSyncs = concatMap (sync :) . chunksOf batchSize
    toBytes = B.toLazyByteString . mconcat . map codewordBE
