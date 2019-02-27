{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer, (>>=), return)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return v = State(\s -> (v, s))
  (State g) >>= f =
    State(\currentState ->
            let (currentValue, newState) = g currentState
                (State h) = f currentValue
            in h newState
         )

instance Monad (Reader s) where
  return v = Reader $ \_ -> v
  (Reader g) >>= f =
    Reader(\r ->
              let v = g r
              in runReader (f v) r
          )

instance Monoid w => Monad (Writer w) where
  return v = Writer $ (mempty, v)
  (Writer (s, v)) >>= f =
    Writer(
              let newW = f v
                  (newS, newV) = runWriter newW
              in (s `mappend`  newS, newV)
          )
