module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

f1 x =
  case x of
    1 -> 2
    2 -> 3
    _ -> 0

f2 1 = 2
f2 2 = 3
f2 _ = 0

root 0 b 0 = (0, 0)
root 0 b c =
  let x = (-c) / b
  in (x, x)
root a b c =
  let d = sqrt( b*b - 4*a*c )
      twice_a = 2 * a
      x1 = (-b + d) / twice_a
      x2 = (-b - d) / twice_a
  in (x1, x2)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib ( n - 2 ) + fib ( n - 1 )

fibm :: Int -> Int
fibm n = fibms !! n

fibms = 0 : 1 : zipWith (+) fibms (drop 1 fibms)
