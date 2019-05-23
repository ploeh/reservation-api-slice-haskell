{-#LANGUAGE OverloadedStrings #-}
module ReservationSQL where

import Control.Exception.Base (bracket)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text
import Data.UUID
import Database.ODBC.SQLServer
import ReservationAPI

toMixedEndianByteString :: UUID -> ByteString
toMixedEndianByteString uuid =
  case BS.unpack $ toByteString uuid of
    [w0,w1,w2,w3, w4,w5, w6,w7, w8,w9, wa,wb,wc,wd,we,wf] ->
      BS.pack [w3,w2,w1,w0, w5,w4, w7,w6, w8,w9, wa,wb,wc,wd,we,wf]
    _ -> BS.empty

insertReservation :: Text -> Reservation -> IO ()
insertReservation connStr (Reservation rid d n e q) =
  let rid' = toSql $ Binary $ BS.toStrict $ toMixedEndianByteString rid
      d' = toSql $ Datetime2 d
      n' = toSql $ pack n
      e' = toSql $ pack e
      q' = toSql q
      sql =
        "INSERT INTO [dbo].[Reservations] ([Guid], [Date], [Name], [Email], [Quantity])\
        \VALUES (" <> rid' <> ", " <> d' <> ", " <> n' <> ", " <> e' <> ", " <> q' <> ")"
  in bracket (connect connStr) close (`exec` sql)