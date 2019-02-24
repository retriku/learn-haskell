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

--SPair
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
curry f = error ""

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = error "uncurry"


-- SMaybe
toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing (\a -> Just a)

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe(\e _ -> e)
fromMaybe (Just v) = SMaybe(\_ f -> f v)

isJust :: SMaybe a -> Bool
isJust (SMaybe m) = m False (\_ -> True)

isNothing :: SMaybe a -> Bool
isNothing (SMaybe m) = m True (\_ -> False)

catMaybes :: SList (SMaybe a) -> SList a
catMaybes (SList sl) = sl empty (\(SMaybe x) xs -> (x empty (\v -> v `cons` empty)) `concat` (catMaybes xs) )
  where empty = fromList []


-- SEither
toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left v) = SEither(\l _ -> l v)
fromEither (Right v) = SEither(\_ r -> r v)

isLeft :: SEither a b -> Bool
isLeft (SEither e) = e (\_ -> True) (\_ -> False)

isRight :: SEither a b -> Bool
isRight (SEither e) = e (\_ -> False) (\_ -> True)

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = error "partition"


-- SList
toList :: SList a -> [a]
toList (SList sl) = sl [] (\x xs -> x : (toList xs))

fromList :: [a] -> SList a
fromList [] = SList(\ni _ -> ni)
fromList (x:xs) = SList(\_ co -> co x (fromList xs))

cons :: a -> SList a -> SList a
cons x xs = SList (\_ co -> co x xs)

concat :: SList a -> SList a -> SList a
concat (SList sl1) (SList sl2) = sl2 (SList sl1) (\_ _ -> sl1 (SList sl2) (\x xs -> x `cons` (xs `concat` (SList sl2))))

null :: SList a -> Bool
null (SList sl) = sl True (\_ _ -> False)

length :: SList a -> Int
length (SList sl) = sl 0 (\_ xs -> 1 + length xs)

map :: (a -> b) -> SList a -> SList b
map f (SList sl) = sl (fromList []) (\x xs -> (f x) `cons` (map f xs))

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList sl1) (SList sl2) = sl1 empty (\x1 xs1 -> sl2 empty (\x2 xs2 -> (fromPair (x1, x2) `cons` zip xs1 xs2)))
  where empty = fromList []

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f e (SList sl) = error ""

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f e (SList sl)= error "foldr"

take :: Int -> SList a -> SList a
take i = error "take"
