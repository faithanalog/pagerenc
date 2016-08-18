module Data.POCSAG
  (transmission)
  where

import Data.List
import Data.Bits
import Data.BitsExtra
import Data.Word
import Data.CRC
import Data.Chunks
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (Builder)

-- POCSAG constants

-- POCSAG SYNC codeword
sync = Codeword 0x7CD215D8

-- POCSAG IDLE codeword
idle = Codeword 0x7A89C197

-- POCSAG Preamble of 576 bits, repeating 1,0,1,0 .. 
preamble = replicate (div 576 32) (Codeword 0xAAAAAAAA)

-- First bit of a message codeword is 1
msgFlag = 0x100000

-- First bit of an address codeword is 0
addrFlag = 0x000000

-- 0x3 in the low two bits of a POCSAG address indicates text data
textFlag = 0x3

-- 0x0 in the low two bits of a POCSAG address indicates numeric data
numericFlag = 0x0

-- Number of data bits per codeword
msgSize = 20

-- Number of words in a frame
frameSize = 2

-- Number of words in a batch
batchSize = 16

-- Fully encoded codeword with CRC added
newtype Codeword = Codeword { getCodeword :: Word32 }

codewordBE :: Codeword -> Builder
codewordBE (Codeword x) = B.word32BE x

-- Takes the 21 bit data segment of a codeword and adds the CRC and parity
-- error correction codes
codeword :: Word32 -> Codeword
codeword x = Codeword (shiftL fullCRC 1 .|. parityBit fullCRC)
  where
    fullCRC = shiftL x crcLength .|. crc x

-- Encode an ASCII message to a series of codewords
encodeASCII :: String -> [Codeword]
encodeASCII = map mkCodeword . chunksOf msgSize . concatMap charToBits
  where
    charToBits = toBitsLE 7 . (fromIntegral . fromEnum :: Char -> Word8)
    mkCodeword bs = codeword (msgFlag .|. fromBitsBE msgSize bs)

transmission :: Word32 -> String -> ByteString
transmission addr txt = toBytes $ (preamble ++ insertSyncs trData)
  where
    addrFrameNum = fromIntegral $ (addr .&. 0x7)
    addrWord = 
        codeword $ (shiftL (shiftR addr 3) 2) .|. textFlag .|. addrFlag
    trData = 
        concat
            [ replicate (addrFrameNum * frameSize) idle
            , [addrWord]
            , encodeASCII txt
            , [idle]]
    insertSyncs = 
        -- Insert syncs, and also pad each batch to 16 words
        concatMap
            (\x -> 
                  sync : x) .
        chunksOf batchSize
    toBytes = B.toLazyByteString . mconcat . map codewordBE
