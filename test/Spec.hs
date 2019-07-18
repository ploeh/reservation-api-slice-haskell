module Main (main) where

import Test.Framework (defaultMain, testGroup)
import ValidationSpec
import ReservationAPISpec
import Repros

main :: IO ()
main =
  defaultMain
    [
      testGroup "Validating" validationTests,
      testGroup "Reservations" reservationAPITests,
      testGroup "Repros" repros
    ]