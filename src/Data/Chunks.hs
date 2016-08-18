module Data.Chunks where

import Data.List
import Data.Int
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null t = [h]
  | otherwise = h : chunksOf n t
  where
    (h,t) = splitAt n xs

chunksOfB :: Int64 -> ByteString -> [ByteString]
chunksOfB n xs
  | B.null t = [xs]
  | otherwise = h : chunksOfB n t
  where
    (h,t) = B.splitAt n xs
