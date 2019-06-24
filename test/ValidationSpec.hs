{-# LANGUAGE ScopedTypeVariables #-}
module ValidationSpec where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import ReservationAPI

validationTests :: [Test]
validationTests =
  [
    testGroup "Validating" [
      testProperty "smaller less than larger returns correct result" $ \
        (smaller :: Int) (Positive diff) -> do
        let larger = smaller + diff
        let actual = validateLessThan "" smaller larger
        Right larger === actual
      ,
      testProperty "larger or same less than smaller returns correct result" $ \
        (smaller :: Int) (NonNegative diff) (expected :: String) -> do
        let larger = smaller + diff
        let actual = validateLessThan expected larger smaller
        Left expected === actual
    ]
  ]