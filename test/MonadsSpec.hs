{-# LANGUAGE NoImplicitPrelude #-}
module MonadsSpec where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer, (>>=), return)
import Monads as M

import Test.Hspec


-- Dummy test to make sure your code type checks before submission.
spec = do
  describe "Lifting values to monads" $ do
    it "should lift Identity" $ do
      M.return 'q' `shouldBe` (M.Identity 'q')
    it "should lift Maybe" $ do
      M.return 'q' `shouldBe` (M.Just 'q')
    it "should lift State" $ do
      (v `shouldBe` "abc")
        where (v, _) = runState (M.return "abc")

type Stack = [Int]
popS :: State Stack Int  
popS = State $ \(x:xs) -> (x,xs)  
  
pushS :: Int -> State Stack ()  
pushS a = State $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = pushS 4 >>= \_ -> pushS 3 >>= (\_ -> popS)
