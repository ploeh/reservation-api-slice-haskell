{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReservationAPISpec where

import Data.UUID
import Data.Aeson
import Test.Hspec
import Test.QuickCheck
import ReservationAPI

instance Arbitrary Reservation where
  arbitrary = do
    (w1, w2, w3, w4) <- arbitrary
    let rid = fromWords w1 w2 w3 w4
    rd <- arbitrary
    rn <- arbitrary
    re <- arbitrary
    rq <- arbitrary
    return $ Reservation rid rd rn re rq

reservationAPISpec :: Spec
reservationAPISpec = do
  describe "Reservation JSON" $ do
    it "renders correctly" $ do
      let rid = fromWords 872411231 2362592316 2161598850 3450687078
      let json = encode $ Reservation rid "2019-04-12" "Jo" "j@example.com" 3
      json `shouldBe` "{\"id\":\"33fff05f-8cd2-4c3c-80d7-6182cdad4e66\",\"date\":\"2019-04-12\",\"name\":\"Jo\",\"email\":\"j@example.com\",\"quantity\":3}"
    it "round-trips" $ property $ \(expected :: Reservation) -> do
      let json = encode $ expected
      let actual = decode json
      actual `shouldBe` Just expected