module Main (main) where

import Test.Hspec
import ValidationSpec
import ReservationAPISpec

main :: IO ()
main = hspec $ do
  validationSpec
  reservationAPISpec