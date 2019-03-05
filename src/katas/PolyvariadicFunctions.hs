{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module PolyvariadicFunctions(
  polyAdd,
  polyList,
  polyWords
  ) where

class PolyAdderInt a where
  addI :: Int -> a
instance PolyAdderInt Int where
  addI = id
instance (Integral a, PolyAdderInt r) => PolyAdderInt (a -> r) where
  addI x = addI . (x +) . fromIntegral

class PolyAdderStr a where
  addS :: String -> a
instance PolyAdderStr String  where
  addS = id
instance PolyAdderStr r => PolyAdderStr (String -> r) where
  addS s1 s2 = addS(s1 ++ d ++ s2)
    where d = if (null s1) then "" else " "

class PolyAdderList a r | r -> a  where
  addL :: [a] -> r
instance PolyAdderList a [a] where
  addL = id
instance PolyAdderList a r => PolyAdderList a (a -> r) where
  addL r e = addL(r ++ [e])


polyAdd :: PolyAdderInt r => r
polyAdd = addI 0

polyWords :: PolyAdderStr r => r
polyWords = addS ""

polyList :: PolyAdderList a r => r
polyList = addL []
