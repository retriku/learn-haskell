{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair = error "toPair"
fromPair :: (a,b) -> SPair a b
fromPair = error "fromPair"
fst :: SPair a b -> a
fst = error "fst"
snd :: SPair a b -> b
snd = error "snd"
swap :: SPair a b -> SPair b a
swap = error "swap"
curry :: (SPair a b -> c) -> (a -> b -> c)
curry = error "curry"
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry = error "uncurry"

toMaybe :: SMaybe a -> Maybe a
toMaybe = error "toMaybe"
fromMaybe :: Maybe a -> SMaybe a
fromMaybe = error "fromMaybe"
isJust :: SMaybe a -> Bool
isJust = error "isJust"
isNothing :: SMaybe a -> Bool
isNothing = error "isNothing"
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = error "catMaybes"

toEither :: SEither a b -> Either a b
toEither = error "toEither"
fromEither :: Either a b -> SEither a b
fromEither = error "fromEither"
isLeft :: SEither a b -> Bool
isLeft = error "isLeft"
isRight :: SEither a b -> Bool
isRight = error "isRight"
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = error "partition"

toList :: SList a -> [a]
toList = error "toList"
fromList :: [a] -> SList a
fromList = error "fromList"
cons :: a -> SList a -> SList a
cons = error "cons"
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
