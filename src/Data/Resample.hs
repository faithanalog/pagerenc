module Data.Resample (resample) where  

import Data.Ratio
import Data.Semigroup

-- | Resample using a nearest-neighbor-ish method
--
-- I feel I owe some explanation to how this works...
--
-- Basically, a series of drops or yields occur for one cycle of resampling.
-- A cycle completes once the ratio of consumed input samples to generated
-- output samples is equal to src / dst. At that point, restarting with both
-- indices at 0 is equivalent to continuing on, so the cycle restarts at 0 0.
-- This is done to prevent integer overflows in long data streams.
--
-- The caller supplies an uncons function which provides the means for
-- termination: The uncons can return a terminating element rather than
-- applying the supplied continuation to stop resampling. Generally this
-- is done once the end of input is reached.
resample ::
     Semigroup m
  => (t -> (m -> t -> m) -> m)
  -> Int
  -> Int
  -> t
  -> m
resample uncons src dst = go 0 0
  where
    (l, m) = simplify src dst
    outIndex x = x * l `div` m
    go i o xs
      | outIndex o > i = uncons xs $ \_ t -> go (i + 1) o t
      | o == m = go 0 0 xs
      | otherwise = uncons xs $ \h _ -> h <> go i (o + 1) xs

simplify :: Int -> Int -> (Int, Int)
simplify n d = (numerator r, denominator r)
  where
    r = n % d
