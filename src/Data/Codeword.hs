module Data.Codeword
  ( Codeword(..)
  , codeword
  ) where

import Data.CRC
import Data.Bits
import Data.Word

newtype Codeword = Codeword { getCodeword :: Word32 }

-- Takes the 21 bit data segment of a codeword and adds the CRC and parity
-- error correction codes
codeword :: Word32 -> Codeword
codeword x = Codeword $ shiftL fullCRC 1 .|. parityBit fullCRC
  where
    fullCRC = shiftL x crcLength .|. crc x
