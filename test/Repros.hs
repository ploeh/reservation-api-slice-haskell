{-# LANGUAGE OverloadedStrings #-}
module Repros where

import Data.Time
import Data.UUID
import Test.Framework (Test)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base hiding (Test, Testable)
import ReservationAPI
import Log

repros :: [Test]
repros = hUnitTestToTests $ TestList [
  "reproduces recorded behaviour" ~: do
    -- These log entries were originally produced by interacting with the
    -- running web application. The only change is that quotes have been
    -- escaped.
    let l = "LogEntry {logTime = 2019-07-18 13:12:28.1716924 UTC, logOperation = \"CurrentTime\", logInput = \"()\", logOutput = \"2019-07-18 15:12:28.1716924\"}\n\
            \LogEntry {logTime = 2019-07-18 13:12:28.702896 UTC, logOperation = \"ReadReservations\", logInput = \"(2019-09-09 15:30:00,2019-09-09 20:30:00)\", logOutput = \"[Reservation {reservationId = 737b1142-d2c2-43d5-a405-4a478a04ea1f, reservationDate = 2019-09-09 17:55:00, reservationName = \\\"Colin Phillips\\\", reservationEmail = \\\"coph@example.com\\\", reservationQuantity = 1},Reservation {reservationId = 0b8a6e8f-05e9-4fc6-9ccd-e2c42fc342a9, reservationDate = 2019-09-09 18:00:00, reservationName = \\\"Anders Jonsen\\\", reservationEmail = \\\"aj@example.org\\\", reservationQuantity = 1},Reservation {reservationId = 909925eb-6c34-40a6-aa7b-d253675dd461, reservationDate = 2019-09-09 18:10:00, reservationName = \\\"Johan Johansson\\\", reservationEmail = \\\"jojo@example.net\\\", reservationQuantity = 2},Reservation {reservationId = 30446bc4-4b7e-43f1-90d7-597da9411e14, reservationDate = 2019-09-09 18:10:00, reservationName = \\\"Burt Catherinos\\\", reservationEmail = \\\"katebu@example.com\\\", reservationQuantity = 2}]\"}"
    let rd = readReplayData $ lines l
    -- Seating duration as configured at the time when the log was captured.
    let seatingDuration = 9000 :: NominalDiffTime
    -- Table configuration at the time when the log was captured.
    let tables =
          [ Table { tableSeats = 2 },
            Table { tableSeats = 4 },
            Table { tableSeats = 6 },
            Table { tableSeats = 8 } ]
    -- This is the reservation attempt that caused the above log entries to be
    -- recorded.
    let r = Reservation
              (fromWords 1802570057 2477539978 3075598441 658055368)
              (LocalTime (fromGregorian 2019 9 9) (TimeOfDay 18 0 0))
              "Ivan Solar"
              "solinv@example.net"
              1

    let actual = replay rd $ tryAccept seatingDuration tables r

    Left (ExecutionError "No table available") ~=? actual
  ]