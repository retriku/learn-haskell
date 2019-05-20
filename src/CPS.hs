module CPS where

import Control.Monad
import Control.Applicative

add :: Int -> Int -> Int
add = (+)

sqr :: Int -> Int
sqr x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (sqr x) (sqr y)

-- CPS
add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps a b = \k -> k (add a b)

sqr_cps :: Int -> ((Int -> r) -> r)
sqr_cps x = \k -> k (sqr x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
  sqr_cps x $ \x_squared ->
  sqr_cps y $ \y_squared ->
  add_cps x_squared y_squared $ k

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS c f = \k -> c (\x -> f x k)

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap f c = Cont $ \k -> runCont c $ \x -> k $ f x

instance Applicative (Cont r) where
  pure a = Cont ($ a)
  liftA2 f fa fb = Cont $ \k ->
    runCont fa $ \x -> (runCont fb $ \y -> k $ f x y)

instance Monad (Cont r) where
  return a = Cont ($ a)
  c >>= f = Cont $ \k -> runCont c $ \x -> runCont (f x) k
