module CPS where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class

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

-- Cont monad
newtype ContM r a = ContM { runContM :: (a -> r) -> r }

instance Functor (ContM r) where
  fmap f c = ContM $ \k -> runContM c $ \x -> k $ f x

instance Applicative (ContM r) where
  pure a = ContM ($ a)
  liftA2 f fa fb = ContM $ \k ->
    runContM fa $ \x -> (runContM fb $ \y -> k $ f x y)

instance Monad (ContM r) where
  return a = ContM ($ a)
  c >>= f = ContM $ \k -> runContM c $ \x -> runContM (f x) k

add_cont :: Int -> Int -> ContM r Int
add_cont a b = return $ a + b

sqr_cont :: Int -> ContM r Int
sqr_cont x = return $ x * x

pythagoras_cont :: Int -> Int -> ContM r Int
pythagoras_cont x y = do
  xx <- sqr_cont x
  yy <- sqr_cont y
  add_cont xx yy

-- ContTr monad transformer
newtype ContTr r m a = ContTr { runContTr :: (a -> m r) -> m r }

instance Functor (ContTr r m) where
  fmap f ct = ContTr $ \c -> runContTr ct (c . f)

instance Applicative (ContTr r m) where
  pure a = ContTr ($ a)
  ctf <*> ctv = ContTr (\c ->
                          runContTr ctf (\f ->
                                           runContTr ctv (c . f)
                                        )
                       )

instance Monad (ContTr r m) where
  return = pure
  a >>= f = undefined

instance MonadTrans (ContTr r) where
  lift = undefined
