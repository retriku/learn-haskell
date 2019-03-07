{-# LANGUAGE
  FlexibleInstances,
  UndecidableInstances,
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 a = isoFunc a $ isoFunc a a

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (
  \ac -> cd . ac . ba,
  \bd -> dc . bd . ab
  )

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero = O
  successor = S
  nat a _ O = a
  nat _ f (S s) = f s
  iter a _ O = a
  iter a f (S s) = iter (f a) f s
  plus O n = n
  plus (S s) n = s + successor n
  minus O _ = O
  minus n O = n
  minus (S s1) (S s2) = s1 - s2
  mult O _ = O
  mult _ O = O
  mult n (S O) = n
  mult n m = multInner n m
    where multInner acc (S O) = acc
          multInner acc (S i) = multInner (acc + n) i
  pow _ O = successor O
  pow O _ = O
  pow n (S O) = n
  pow n m = powInner n m
    where powInner acc (S O) = acc
          powInner acc (S i) = powInner (acc * n) i

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (),
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero = []
  successor = (():)
  nat a _ [] = a
  nat _ f (_:xs) = f xs
  iter a _ [] = a
  iter a f (_:xs) = iter (f a) f xs
  plus [] n = n
  plus (x:xs) n = xs + successor n
  minus [] _ = []
  minus n [] = n
  minus (_:xs) (_:ys) = xs - ys
  mult [] _ = []
  mult _ [] = []
  mult n [()] = n
  mult n m = multInner n m
    where multInner acc [()] = acc
          multInner acc (_:xs) = multInner (acc + n) xs
  pow _ [] = successor []
  pow [] _ = []
  pow n [()] = n
  pow n m = powInner n m
    where powInner acc [()] = acc
          powInner acc (_:xs) = powInner (acc * n) xs

-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  zero = Scott(\z _ -> z)
  successor n = Scott(\_ s -> s n)
  nat a f (Scott s) = s a f
  iter a f (Scott s) = s a (iter (f a) f)
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero = Church(\_ z -> z)
  successor (Church n) = Church(\c z -> n c (c z))
  nat a f c@(Church n) = n (\_ -> f (c - one)) a
    where one = successor zero
  iter a f (Church n) = n f a
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow
