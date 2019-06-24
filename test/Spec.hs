module Main (main) where

import Test.Framework (defaultMain)
import ValidationSpec
import ReservationAPISpec

main :: IO ()
main = defaultMain (validationTests ++ reservationAPITests)