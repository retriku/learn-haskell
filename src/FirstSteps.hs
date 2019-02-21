{-# LANGUAGE DeriveFoldable #-}
module FirstSteps where

import qualified Data.Text.IO as T
import Acme.Missiles
import Control.Monad.Except
import Data.List

someFunc :: IO ()
someFunc = launchMissiles

makeList = 1 : makeList


data MyError = EmptyLine

instance Show MyError where
  show EmptyLine = "EmptyLine"

mightFail :: ExceptT MyError IO ()
mightFail = do
  l <- lift getLine
  when (null l) (throwError EmptyLine)


fibs1 n = (unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)) !! n

fibs2 n = fibs !! n 
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs3 n = snd $ (iterate (next) (0, 1)) !! (n - 1)
  where next (a, b) = (b, a + b)

 
data MyList a = MyEmpty | MyCons a (MyList a)

listHead :: MyList a -> Maybe a
listHead MyEmpty = Nothing
listHead (MyCons a _) = Just a

listTail :: MyList a -> MyList a
listTail MyEmpty = MyEmpty
listTail (MyCons _ tail) = tail

listFoldl :: (a -> b -> a) -> a -> MyList b -> a
listFoldl _ z MyEmpty = z
listFoldl f z (MyCons e tail) =
  listFoldl f (f z e) tail

listFoldr :: (a -> b -> b) -> b -> MyList a -> b
listFoldr _ z MyEmpty = z
listFoldr f z (MyCons e tail) =
  f e $ listFoldr f z tail


data BinaryTree a = Leaf a | Branch (BinaryTree a) a (BinaryTree a) deriving Foldable

elements :: BinaryTree a -> [a]
elements (Leaf a) = [a]
elements (Branch t1 e t2) =
  (elements t1) ++ [e] ++ (elements t2)

tree = Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Branch (Leaf 5) 6 (Leaf 7))

treeFold :: (a -> b -> b) -> b -> BinaryTree a -> b
treeFold f z (Leaf e) = f e z
treeFold f z (Branch t1 e t2) =
  treeFold f (f e leftFold) t1
  where
    leftFold = treeFold f z t2
