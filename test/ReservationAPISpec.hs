{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReservationAPISpec where

import Control.Monad
import Data.Coerce
import Data.UUID
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Aeson
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Instances.UUID
import ReservationAPI

instance Arbitrary ZonedTimeStructEq where
  arbitrary = ZonedTimeStructEq <$> arbitrary

instance Arbitrary Reservation where
  arbitrary =
    liftM5 Reservation arbitrary arbitrary arbitrary arbitrary arbitrary

-- The time zone flag and names aren't preserved when rendered as JSON
makeZonedTimeRoundTrippable :: ZonedTimeStructEq -> ZonedTimeStructEq
makeZonedTimeRoundTrippable (ZonedTimeStructEq (ZonedTime lt tz)) =
  coerce $ ZonedTime lt $ tz { timeZoneSummerOnly = False, timeZoneName = "" }

reservationAPISpec :: Spec
reservationAPISpec = do
  describe "Reservation JSON" $ do
    it "renders correctly" $ do
      let rid = fromWords 872411231 2362592316 2161598850 3450687078
      let rd = coerce $ ZonedTime
            (LocalTime (fromGregorian 2019 4 12) (TimeOfDay 19 0 0))
            (hoursToTimeZone 2)

      let json = encode $ Reservation rid rd "Jo" "j@example.com" 3

      json `shouldBe` "{\"id\":\"33fff05f-8cd2-4c3c-80d7-6182cdad4e66\",\
                       \\"date\":\"2019-04-12T19:00:00+02:00\",\
                       \\"name\":\"Jo\",\
                       \\"email\":\"j@example.com\",\
                       \\"quantity\":3}"

    it "round-trips" $ property $ \(r :: Reservation) -> do
      let expected =
            r { reservationDate = makeZonedTimeRoundTrippable $ reservationDate r }
      let json = encode $ expected

      let actual = decode json

      actual `shouldBe` Just expected