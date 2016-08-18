module Data.FLEX
  (transmission)
  where

import Data.List
import Data.Bits
import Data.BitsExtra
import Data.Word
import Data.CRC
import Data.Chunks
import Data.Monoid
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (Builder)

-- Fully encoded codeword with CRC added
newtype Codeword = Codeword { getCodeword :: Word32 }

codewordBE :: Codeword -> Builder
codewordBE (Codeword x) = B.word32BE x

-- Takes the 21 bit data segment of a codeword and adds the CRC and parity
-- error correction codes
codeword :: Word32 -> Codeword
codeword x = Codeword (shiftL fullCRC 1 .|. parityBit fullCRC)
  where
    fullCRC = shiftL msg crcLength .|. crc msg
    -- Flex codewords are like POCSAG codewords, but the data is in the reverse order
    msg = (fromBitsBE 21 . toBitsLE 21) x

data FlexPageType
    = PageTypeSecure 
    | PageTypeUnknown 
    | PageTypeTone 
    | PageTypeStandardNumeric 
    | PageTypeSpecialNumeric 
    | PageTypeAlphaNumeric 
    | PageTypeBinary 
    | PageTypeNumberedNumeric
    deriving (Enum)

encodeASCII :: String -> [Codeword]
encodeASCII = (codeword 0 :) . map codeword . go . map ascii
  where
    go [] = []
    go [a] = go [a, 0x03, 0x03]
    go [a,b] = go [a, b, 0x03]
    go (a:b:c:xs) = (shiftL c 14 .|. shiftL b 7 .|. a) : go xs
    ascii x = (fromIntegral . fromEnum) x .&. 0x7F
    


checksum :: Word32 -> Word32
checksum x = 
    0xF -
    (sum
         [ shiftR x n .&. 0xF
         | n <- [4,8 .. 20] ] .&.
     0xF)

withChecksum :: Word32 -> Word32
withChecksum x = x .|. checksum x


sync1 :: Builder
sync1 = B.word64BE (complement 0x870CA6C6AAAA78F3) <> B.word16BE 0x0000

frameInfo :: Word32 -> Word32 -> Builder
frameInfo cycle frame = 
    codewordBE . codeword . withChecksum $ shiftL frame 8 .|. shiftL cycle 4

sync2 :: Builder
sync2 = B.word32BE 0 <> B.word8 0

blockInfo :: Word32 -> Word32 -> Codeword
blockInfo ai vi = codeword . withChecksum $ shiftL vi 10 .|. shiftL ai 8

address :: Word32 -> Codeword
address addr = codeword (addr + 0x8000)

vector :: String -> Codeword
vector txt = 
    codeword . withChecksum $
    shiftL len 14 .|. shiftL offset 7 .|. shiftL msgType 4
  where
    offset = 3
    len = 1 + fromIntegral ((length txt + 2) `div` 3)
    msgType = (fromIntegral . fromEnum) PageTypeAlphaNumeric

interleaveWords :: [Codeword] -> [Word32]
interleaveWords = 
    map (fromBitsBE 32) .
    chunksOf 32 .
    concat . concatMap transpose . chunksOf 8 . map (toBitsBE 32 . getCodeword)

idle :: [Codeword]
-- We switch between a positive and negative idle because
-- multimon will believe it lost its lock if data does not have a zero
-- crossing within a certain timeframe
idle = concat (repeat [codeword 0x1FFFFF, codeword 0])

transmission :: Word32 -> String -> ByteString
transmission addr txt = 
    B.toLazyByteString $
    trHeader <> (mconcat . map B.word32BE . interleaveWords) trData
  where
    cycle = 0
    frame = 0
    addrIndex = 0
    vectorIndex = 2
    trHeader = mconcat [sync1, frameInfo cycle frame, sync2]
    trData = 
        take 88 $
        [blockInfo addrIndex vectorIndex, address addr, vector txt] ++
        encodeASCII txt ++ idle
