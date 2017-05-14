module Data.BitsExtra where

import Data.Bits
import Data.List

type Bit = Bool

toBitsBE :: Bits b => Int -> b -> [Bit]
toBitsBE n x = map (testBit x) [n - 1,n - 2 .. 0]

toBitsLE :: Bits b => Int -> b -> [Bit]
toBitsLE n x = map (testBit x) [0 .. n - 1]

fromBitsBE :: (Bits b, Num b) => Int -> [Bit] -> b
fromBitsBE n bs =
  foldl' (.|.) 0 $ zipWith (\b i -> shiftL (bitFor b) i) bs [n - 1,n - 2 .. 0]
  where
    bitFor True = 1
    bitFor False = 0
