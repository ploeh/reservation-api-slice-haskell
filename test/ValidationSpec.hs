{-# LANGUAGE ScopedTypeVariables #-}
module ValidationSpec where

import Test.Hspec
import Test.QuickCheck
import ReservationAPI

validationSpec :: Spec
validationSpec = describe "Validating" $ do
  it "smaller less than larger returns correct result" $ property $ \
    (smaller :: Int) (Positive diff) -> do
    let larger = smaller + diff
    let actual = validateLessThan "" smaller larger
    actual `shouldBe` Right larger

  it "larger or same less than smaller returns correct result" $ property $ \
    (smaller :: Int) (NonNegative diff) (expected :: String) -> do
    let larger = smaller + diff
    let actual = validateLessThan expected larger smaller
    actual `shouldBe` Left expected
