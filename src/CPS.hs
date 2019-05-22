module CPS where

import Control.Monad
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Identity

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
  ct >>= f =
    ContTr $ \k ->
               runContTr ct $ \c ->
                                runContTr (f c) k

instance MonadTrans (ContTr r) where
  lift ma = ContTr $ \k -> ma >>= k

-- Cont monad
--newtype ContM r a = ContM { runContM :: (a -> r) -> r }

--instance Functor (ContM r) where
--  fmap f c = ContM $ \k -> runContM c $ \x -> k $ f x

--instance Applicative (ContM r) where
--  pure a = ContM ($ a)
--  liftA2 f fa fb = ContM $ \k ->
--    runContM fa $ \x -> (runContM fb $ \y -> k $ f x y)

--instance Monad (ContM r) where
--  return a = ContM ($ a)
--  c >>= f = ContM $ \k -> runContM c $ \x -> runContM (f x) k

type ContM r = ContTr r Identity

contM :: ((a -> r) -> r) -> ContM r a
contM f = ContTr $ \c -> Identity(f $ runIdentity . c)

runContM :: ContM r a -> (a -> r) -> r
runContM r f = runIdentity (runContTr r $ Identity . f)

callCC' :: ((a -> ContM r b) -> ContM r a) -> ContM r a
callCC' c = undefined

divExcpt :: Int -> Int -> (String -> ContM r Int) -> ContM r Int
divExcpt x y handler = callCC' $ \ok -> do
    err <- callCC' $ \notOk -> do
        when (y == 0) $ notOk "Denominator 0"
        ok $ x `div` y
    handler err


add_cont :: Int -> Int -> ContM r Int
add_cont a b = return $ a + b

sqr_cont :: Int -> ContM r Int
sqr_cont x = return $ x * x

pythagoras_cont :: Int -> Int -> ContM r Int
pythagoras_cont x y = do
  xx <- sqr_cont x
  yy <- sqr_cont y
  add_cont xx yy

-- from https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
bar :: Char -> String -> Cont r Int
bar c s = do
    msg <- callCC $ \k -> do
        let s0 = c : s
        when (s0 == "hello") $ k "They say hello."
        let s1 = show s0
        return ("They appear to be saying " ++ s1)
    return (length msg)
