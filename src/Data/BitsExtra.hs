module Data.BitsExtra
  ( toBitsBE
  , toBitsLE
  , fromBitsBE
  ) where

import Data.Bits
import Data.List

toBitsBE :: Bits b => Int -> b -> [Bool]
toBitsBE n x =
  unfoldr
    (\i ->
       if i < 0
         then Nothing
         else Just (testBit x i, i - 1))
    (n - 1)

toBitsLE :: Bits b => Int -> b -> [Bool]
toBitsLE n x =
  unfoldr
    (\i ->
       if i == n
         then Nothing
         else Just (testBit x i, i + 1))
    0

fromBitsBE :: (Bits b, Num b) => Int -> [Bool] -> b
fromBitsBE n =
  snd .
  foldl' (\(i, x) b -> (i - 1, x .|. shiftL (bitFor b) i)) (n - 1, 0) . take n
  where
    bitFor True = 1
    bitFor False = 0
