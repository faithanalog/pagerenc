module Data.FLEX
  (transmission)
  where

import Data.Bits
import Data.BitsExtra
import Data.List
import Data.List.Split
import Data.Monoid
import Data.Word
import Data.Foldable
import Data.Codeword hiding (codeword)
import qualified Data.Codeword as Codeword

-- FLEX codewords have the data flipped
codeword :: Word32 -> Codeword
codeword = Codeword.codeword . fromBitsBE 21 . toBitsLE 21

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
encodeASCII =
  (codeword 0 :) . map (codeword . groupChars) . chunksOf 3 . map ascii
  where
    groupChars =
      foldr (\x acc -> x .|. shiftL acc 7) 0x03 -- Iniital value of ETX (End Of Text) for padding
    ascii x = fromIntegral (fromEnum x) .&. 0x7F

checksum :: Word32 -> Word32
checksum x = 0xF - (sum nibbles .&. 0xF)
  where
    nibbles = map (\n -> shiftR x n .&. 0xF) [4,8 .. 20]

withChecksum :: Word32 -> Word32
withChecksum x = x .|. checksum x

sync1 :: [Bool]
sync1 = toBitsBE 64 (complement 0x870CA6C6AAAA78F3 :: Word64) <> toBitsBE 16 (0x0000 :: Word16)

frameInfo :: Word32 -> Word32 -> Codeword
frameInfo cycle frame =
  codeword . withChecksum $ shiftL frame 8 .|. shiftL cycle 4

sync2 :: [Bool]
sync2 = toBitsBE 40 (0 :: Word64)

blockInfo :: Word32 -> Word32 -> Codeword
blockInfo ai vi = codeword . withChecksum $ shiftL vi 10 .|. shiftL ai 8

address :: Word32 -> Codeword
address addr = codeword $ addr + 0x8000

vector :: String -> Codeword
vector txt =
  codeword . withChecksum $
  shiftL len 14 .|. shiftL offset 7 .|. shiftL msgType 4
  where
    offset = 3
    len = 1 + fromIntegral ((length txt + 2) `div` 3)
    msgType = fromIntegral $ fromEnum PageTypeAlphaNumeric

interleaveWords :: [Bool] -> [Bool]
interleaveWords = fold . (foldMap transpose . chunksOf 8) . chunksOf 32

idle :: [Codeword]
-- We switch between a positive and negative idle because
-- multimon will believe it lost its lock if data does not have a zero
-- crossing within a certain timeframe
idle = cycle [codeword 0x1FFFFF, codeword 0]

transmission :: Word32 -> String -> [Bool]
transmission addr txt =
  trHeader <> (interleaveWords . foldMap (toBitsBE 32 . getCodeword) $ trData)
  where
    cycle = 0
    frame = 0
    addrIndex = 0
    vectorIndex = 2
    trHeader =
      fold [sync1, toBitsBE 32 . getCodeword $ frameInfo cycle frame, sync2]
    trData =
      take 88 $
      [blockInfo addrIndex vectorIndex, address addr, vector txt] ++
      encodeASCII txt ++ idle
