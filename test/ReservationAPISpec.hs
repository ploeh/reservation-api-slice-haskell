{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReservationAPISpec where

import Control.Monad
import Data.Aeson
import Test.Hspec
import Test.QuickCheck
import ReservationAPI

instance Arbitrary Reservation where
  arbitrary = liftM4 Reservation arbitrary arbitrary arbitrary arbitrary

reservationAPISpec :: Spec
reservationAPISpec = do
  describe "Reservation JSON" $ do
    it "renders correctly" $ do
      let json = encode $ Reservation "2019-04-12" "Jo" "j@example.com" 3
      json `shouldBe` "{\"date\":\"2019-04-12\",\"name\":\"Jo\",\"email\":\"j@example.com\",\"quantity\":3}"
    it "round-trips" $ property $ \(expected :: Reservation) -> do
      let json = encode $ expected
      let actual = decode json
      actual `shouldBe` Just expected