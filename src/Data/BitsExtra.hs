{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
fromBitsBE n xs =
  foldl' (.|.) 0 $ zipWith (\i b -> shiftL (from b) i) [n - 1, n - 2 .. 0] xs
  where
    from True = 1
    from False = 0
