module PolyvariadicFunctions(
  polyAdd,
  polyList,
  polyWords
  ) where

class (Monoid a) => PolyAdder a where
  polyAdd :: Int -> a

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0

instance PolyAdder Int where
  polyAdd = id

instance (Integral a, PolyAdder r) => PolyAdder (a -> r) where
  polyAdd x = polyAdd . (mappend x) . fromIntegral

-- `polyAdd` sums its arguments, all `Int`s.
-- polyAdd ::

-- `polyList` turns its arguments into a list, polymorphically.
polyList :: [a] -> [a]
polyList = error "TODO: polyList"

-- `polyWords` turns its arguments into a spaced string.
polyWords :: String -> String
polyWords = error "TODO: polyList"
