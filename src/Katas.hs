{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}
module Katas where

import Control.Applicative
import Data.Functor.Contravariant (Contravariant (..))
import Control.Monad
import Control.Monad.Trans
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

--lift f = unit . f

bindR :: (a -> StdGen -> (b, StdGen)) -> (StdGen -> (a, StdGen)) -> (StdGen -> (b, StdGen))
bindR f r seed = (b, newGen)
  where (a, gen) = r seed
        (b, newGen) = f a gen

-- show
newtype Predicate a = Predicate { getPredicate ∷ a → Bool }

instance Contravariant Predicate where
    contramap g (Predicate p) = Predicate (p . g)

veryOdd ∷ Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

main ∷ IO ()
main = do
  print $ map show [0 .. 11]
  print $ getPredicate veryOdd <$> [0 .. 11]


newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f = Identity . f . runIdentity

instance Applicative Identity where
  pure = Identity
  liftA2 f fa fb =
    let a = runIdentity fa
        b = runIdentity fb
    in Identity $ f a b

instance Monad Identity where
    return = pure
    m >>= k  = k (runIdentity m)

newtype IdentityT m v = IdentityT { runIdentityT :: m (Identity v) }

instance Monad m => Functor (IdentityT m) where
  fmap = liftM

instance Monad m => Applicative (IdentityT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (IdentityT m) where
  return = IdentityT . return . Identity

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  it >>= k = IdentityT $ do
    a <- ma
    runIdentityT $ k a
      where ma = fmap runIdentity $ runIdentityT it

instance MonadTrans IdentityT where
  lift = IdentityT . (liftM Identity)


curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \a -> \b -> f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a, b) -> f a b

addPair = uncurry' (+)
