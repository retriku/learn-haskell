{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Katas where

import Control.Applicative
import Data.Functor.Contravariant (Contravariant (..))
import Control.Monad
import Control.Monad.Trans.Class
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

newtype IdentityT c v = IdentityT { runIdT :: c (Identity v) }

instance Functor f => Functor (IdentityT f) where
  fmap f idT =
    let fid = runIdT idT
    in IdentityT $ fmap (Identity . f . runIdentity) fid

instance Applicative a => Applicative (IdentityT a) where
  pure aidT = undefined
  liftA2 f t1 t2 = undefined

instance Monad m => Monad (IdentityT m) where
  return = IdentityT . return . Identity
  m >>= k = undefined

instance MonadTrans IdentityT where
  lift ma = undefined
