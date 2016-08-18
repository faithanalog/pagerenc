module Data.BitsExtra where

import Data.Bits
import Data.List

type Bit = Bool

toBitsBE :: Bits b => Int -> b -> [Bit]
toBitsBE n x = 
    [ testBit x i
    | i <- [n - 1,n - 2 .. 0] ]

toBitsLE :: Bits b => Int -> b -> [Bit]
toBitsLE n x = 
    [ testBit x i
    | i <- [0 .. n - 1] ]


fromBitsBE :: (Bits b, Num b) => Int -> [Bit] -> b
fromBitsBE n bs = 
    foldl' (.|.) 0 $
    [ shiftL (bitFor b) i
    | (b,i) <- zip bs [n - 1,n - 2 .. 0] ]
  where
    bitFor True = 1
    bitFor False = 0
