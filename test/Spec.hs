module Main (main) where

import Test.Framework (defaultMain, testGroup)
import ValidationSpec
import ReservationAPISpec

main :: IO ()
main =
  defaultMain
    [
      testGroup "Validating" validationTests,
      testGroup "Reservations" reservationAPITests
    ]