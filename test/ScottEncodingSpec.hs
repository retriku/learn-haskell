module ScottEncodingSpec where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))
import ScottEncoding
import Test.Hspec

spec = do
  describe "The Maybe type" $ do
    it "can be cast to Prelude.Maybe" $ do
      toMaybe (SMaybe const) `shouldBe` (Nothing :: Maybe Int)
      toMaybe (SMaybe $ \_ f -> f 4) `shouldBe` Just 4
    it "can be cast from Prelude.Maybe" $ do
      runMaybe (fromMaybe (Nothing)) 0 (+1) `shouldBe` 0
      runMaybe (fromMaybe (Just 4)) 0 (+1) `shouldBe` 5
  describe "The list type" $ do
    it "can be cast to []" $ do
      toList (SList const) `shouldBe` ([] :: [Int])
      toList (SList $ \_ f -> f 1 (SList $ \_ g -> g 2 (SList const))) `shouldBe` [1,2]
    it "can be cast from []" $ do
      runList (fromList []) 0 reduce `shouldBe` 0
      runList (fromList [1,2]) 0 reduce `shouldBe` 21
    it "can calculate SList length" $ do
      (length . fromList) [] `shouldBe` 0
      (length . fromList) [1,2,3] `shouldBe` 3
    it "can map SList a to SList b" $ do
      (toList . (map (*2)) . fromList) [1,2,3] `shouldBe` [2,4,6]
    it "can concat two SLists" $ do
      (toList $ concat (fromList [1,2,3]) (fromList [4,5,6])) `shouldBe` [1,2,3,4,5,6]
      (toList $ concat (fromList []) (fromList [4,5,6])) `shouldBe` [4,5,6]
      (toList $ concat (fromList [1,2,3]) (fromList [])) `shouldBe` [1,2,3]
  describe "The Either type" $ do
    it "can be cast to Prelude.Either" $ do
      toEither (SEither $ \f _ -> f 3) `shouldBe` (Left 3 :: Either Int String)
      toEither (SEither $ \_ f -> f "hello") `shouldBe` (Right "hello" :: Either Int String)
    it "can be cast from Prelude.Either" $ do
      runEither (fromEither (Left 3)) show id `shouldBe` "3"
      runEither (fromEither (Right "hello" :: Either Int String)) show id `shouldBe` "hello"
  describe "The pair type" $ do
    it "can be cast to (,)" $ do
      toPair (SPair $ \f -> f 2 "hi") `shouldBe` (2, "hi")
    it "can be cast from (,)" $ do
      runPair (fromPair (2, "hi")) replicate `shouldBe` ["hi", "hi"]

reduce :: Num a => a -> SList a -> a
reduce i sl = i + 10 * runList sl 0 reduce
