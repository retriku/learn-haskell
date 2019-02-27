{-# LANGUAGE NoImplicitPrelude #-}
module MonadsSpec where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer, (>>=), return)
import Monads as M

import Test.Hspec


spec = do
  describe "Lifting values to monads" $ do
    it "should lift Identity" $ do
      M.return 'q' `shouldBe` (M.Identity 'q')
    it "should lift Maybe" $ do
      M.return 'q' `shouldBe` (M.Just 'q')
    it "should lift State" $ do
      fst(runState (M.return 1) []) `shouldBe` 1
    it "should lift Reader" $ do
      runReader (M.return 1) 2 `shouldBe` 1
    it "should lift Writer" $ do
      runWriter (M.return 1) `shouldBe` ((), 1)

popS :: Show a => State [a] a
popS = State $ \(x:xs) -> (x,xs)  
  
pushS :: Show a =>  a -> State [a] ()  
pushS a = State $ \xs -> ((), a:xs)
