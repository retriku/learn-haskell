{-# LANGUAGE RankNTypes #-}
module Katas where

difference :: Eq a => [a] -> [a] -> [a]
difference a [] = a
difference [] _ = []
difference a (x : xs) =
  difference (filter (\x1 -> x /= x1) a) xs

noBoringZeros :: Int -> Int
noBoringZeros 0 = 0
noBoringZeros a =
  if ((a `mod` 10) == 0) then
    noBoringZeros $ a `div` 10
  else
    a

newtype ListS a =
  ListS {
    unconsS :: forall r. (a -> ListS a -> r) -> r -> r
  }
