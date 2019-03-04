{-# LANGUAGE FlexibleInstances #-}
module PolyvariadicFunctions(
  polyAdd,
  polyList,
  polyWords
  ) where

class PolyAdderInt a where
  addI :: Int -> a

{-
instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
-}

instance PolyAdderInt Int where
  addI = id

instance (Integral a, PolyAdderInt r) => PolyAdderInt (a -> r) where
  addI x = addI . (x +) . fromIntegral

polyAdd :: PolyAdderInt a => a
polyAdd = addI 0

-- `polyAdd` sums its arguments, all `Int`s.
-- polyAdd ::

class PolyAdderList a where
  addL :: [a] -> a

instance PolyAdderList a where
  addL = undefined

-- `polyList` turns its arguments into a list, polymorphically.
polyList :: [a] -> [a]
polyList = error "TODO: polyList"


class PolyAdderStr a where
  addS :: String -> a

instance PolyAdderStr String  where
  addS = id

instance PolyAdderStr r => PolyAdderStr (String -> r) where
  addS s1 s2 = addS(s1 ++ d ++ s2)
    where d = if (null s1) then "" else " "


-- `polyWords` turns its arguments into a spaced string.
polyWords :: PolyAdderStr r => r
polyWords = addS ""
