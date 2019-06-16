module Main (main) where

import Test.Hspec
import ReservationAPISpec

main :: IO ()
main = hspec $ do
  reservationAPISpec