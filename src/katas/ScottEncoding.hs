{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair {
  runPair :: forall c. (a -> b -> c) -> c
  -- runPair :: SPair a b -> (a -> b -> c) -> c
}

toPair :: SPair a b -> (a,b)
toPair (SPair p) = p(\a b -> (a, b))
fromPair :: (a,b) -> SPair a b
fromPair (a,b) = SPair(\p -> p a b)
fst :: SPair a b -> a
fst (SPair p) = p(\a _ -> a) 
snd :: SPair a b -> b
snd (SPair p) = p(\_ b -> b)
swap :: SPair a b -> SPair b a
swap (SPair p) = p(\a b -> SPair (\p -> p b a))
curry :: (SPair a b -> c) -> (a -> b -> c)
curry = error "curry"
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry = error "uncurry"

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing (\a -> Just a)
fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe(\e _ -> e)
fromMaybe (Just v) = SMaybe(\_ f -> f v) 
isJust :: SMaybe a -> Bool
isJust = error "isJust"
isNothing :: SMaybe a -> Bool
isNothing = error "isNothing"
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = error "catMaybes"

toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right
fromEither :: Either a b -> SEither a b
fromEither (Left v) = SEither(\l _ -> l v)
fromEither (Right v) = SEither(\_ r -> r v)
isLeft :: SEither a b -> Bool
isLeft = error "isLeft"
isRight :: SEither a b -> Bool
isRight = error "isRight"
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = error "partition"

toList :: SList a -> [a]
toList (SList l) = l [] (\x xs -> x : (toList xs))
fromList :: [a] -> SList a
fromList [] = SList(\ni _ -> ni)
fromList (x:xs) = SList(\_ co -> co x (fromList xs))
cons :: a -> SList a -> SList a
cons x xs = SList (\_ co -> co x xs)
concat :: SList a -> SList a -> SList a
concat = error "concat"
null :: SList a -> Bool
null = error "null"
length :: SList a -> Int
length = error "length"
map :: (a -> b) -> SList a -> SList b
map = error "map"
zip :: SList a -> SList b -> SList (SPair a b)
zip = error "zip"
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl = error "foldl"
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr = error "foldr"
take :: Int -> SList a -> SList a
take = error "take"
