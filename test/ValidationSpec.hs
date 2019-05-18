{-# LANGUAGE ScopedTypeVariables #-}
module ValidationSpec where

import Test.Hspec
import Test.QuickCheck
import Validation

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validated e a) where
  arbitrary = Validated <$> arbitrary

validationSpec :: SpecWith ()
validationSpec =
  describe "Validated" $ do
    it "obeys the applicative identity law" $ property $ \
      (v :: Validated String Int) ->
      v `shouldBe` pure id <*> v

    it "obeys the applicative composition law" $ property $ do
      u :: Validated Ordering (String -> Ordering) <- arbitrary
      v :: Validated Ordering (Integer -> String) <- arbitrary
      w :: Validated Ordering Integer <- arbitrary

      let left = pure (.) <*> u <*> v <*> w
      let right = u <*> (v <*> w)

      return $ left `shouldBe` right

    it "obeys the applicative homomorphism law" $ property $ do
      f :: String -> Double <- arbitrary
      x :: String <- arbitrary

      let left :: Validated String Double = pure f <*> pure x
      let right = pure (f x)

      return $ left `shouldBe` right

    it "obeys the applicative interchange law" $ property $ do
      u :: Validated [Double] (Integer -> String) <- arbitrary
      y :: Integer <- arbitrary

      let left = u <*> pure y
      let right = pure ($ y) <*> u

      return $ left `shouldBe` right