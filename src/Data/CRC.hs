{-# LANGUAGE BinaryLiterals #-}
module Data.CRC where

import Data.Bits
import Data.List
import Data.Word

-- Length in bits of the CRC code
crcLength :: Int
crcLength = 10

-- Calculate the CRC code for a 21 bit message
crc :: Word32 -> Word32
crc x = foldl' div val [0 .. 20]
  where
     -- msg to encode shifted left size of crc
    val = shiftL x crcLength

    -- generator shifted left to align with msg
    gen = shiftL 0b11101101001 20   

    -- One iteration of the polynomial division for column 'col'
    div dv col
        -- If bit in current column == 1, xor generator with dividend 
      | testBit dv (30 - col) = xor dv (shiftR gen col)
        -- If bit in current column == 0, do nothing
      | otherwise = dv

-- Return 1 if number of bits in a word is odd, 0 if even
-- This is a naive parity implementation. There are more effecient
-- ones but performance is not an issue for this project
parityBit :: Word32 -> Word32
parityBit x = p .&. 1
  where
    p = foldl1' xor [shiftR x b | b <- [0 .. 31]]
