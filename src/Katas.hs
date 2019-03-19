{-# LANGUAGE RankNTypes #-}
module Katas where

import System.Random

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

bind :: (Integer -> (Integer, String)) -> ((Integer, String) -> (Integer, String))
bind f g@(x,s) = (y, s ++ s')
  where (y, s') = f x

unit :: Integer -> (Integer, String)
unit n = (n, "")

lift f = unit . f

bindR :: (a -> StdGen -> (b, StdGen)) -> (StdGen -> (a, StdGen)) -> (StdGen -> (b, StdGen))
bindR f r seed = (b, newGen)
  where (a, gen) = r seed
        (b, newGen) = f a gen
