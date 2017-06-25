{-# LANGUAGE DeriveFunctor #-}
module Data.Resample (resample) where  

import Data.Ratio
import Data.Monoid

-- | This is the Hylomorphism from the recursion-schemes package,
-- re-implemeneted to avoid dependening on the entirety of recursion-schemes.
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = f . fmap (hylo f g) . g
  
data Resample a
  = Done
  | Next a
  | Yield a
  deriving (Functor)

-- | Resample using a nearest-neighbor-ish method
--
-- I feel I owe some explanation to how this works...
--
-- Basically, a list of Next/Yield actions is generated for one cycle of
-- resampling. A cycle completes once the ratio of consumed input samples to
-- generated output samples is equal to src / dst. At that point, restarting
-- with both indices at 0 is equivalent to continuing on. This is done to
-- prevent integer overflows in long data streams.
--
-- The Next/Yield actions are recursively consumed using a continuation passing
-- style in such a way that once the input data is exhausted, the loop will
-- terminate and return mempty.
resample ::
     Monoid m
  => (m -> (m -> t -> m) -> t -> m)
  -> Int
  -> Int
  -> t
  -> m
resample uncons src dst = go
  where
    go = hylo psi phi (0, 0) go
    psi = runResampler uncons
    phi = buildResampler src dst

buildResampler :: Int -> Int -> (Int, Int) -> Resample (Int, Int)
buildResampler src dst (i, o)
  | outIndex o > i = Next (i + 1, o)
  | o == m = Done
  | otherwise = Yield (i, o + 1)
  where
    (l, m) = simplify src dst
    outIndex x = x * l `div` m

runResampler ::
     Monoid m
  => (m -> (m -> t -> m) -> t -> m)
  -> Resample ((t -> m) -> t -> m)
  -> (t -> m)
  -> t
  -> m
runResampler uncons r g xs =
  case r of
    Done -> g xs
    Next f -> flip (uncons mempty) xs $ \_ t -> f g t
    Yield f -> flip (uncons mempty) xs $ \h _ -> h <> f g xs

simplify :: Int -> Int -> (Int, Int)
simplify n d = (numerator r, denominator r)
  where
    r = n % d
