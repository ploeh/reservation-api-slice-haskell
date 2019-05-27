{-#LANGUAGE OverloadedStrings #-}
module ReservationSQL where

import Control.Exception.Base (bracket)
import Data.Coerce
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Database.ODBC.SQLServer
import ReservationAPI

toMixedEndianByteString :: UUID -> ByteString
toMixedEndianByteString uuid =
  case BS.unpack $ toByteString uuid of
    [w0,w1,w2,w3, w4,w5, w6,w7, w8,w9, wa,wb,wc,wd,we,wf] ->
      BS.pack [w3,w2,w1,w0, w5,w4, w7,w6, w8,w9, wa,wb,wc,wd,we,wf]
    _ -> BS.empty

fromMixedEndianByteString :: ByteString -> Maybe UUID
fromMixedEndianByteString bs =
  case BS.unpack bs of
    [w0,w1,w2,w3, w4,w5, w6,w7, w8,w9, wa,wb,wc,wd,we,wf] ->
      fromByteString $
        BS.pack [w3,w2,w1,w0, w5,w4, w7,w6, w8,w9, wa,wb,wc,wd,we,wf]
    _ -> Nothing

insertReservation :: Text -> Reservation -> IO ()
insertReservation connStr (Reservation rid d n e q) =
  let rid' = toSql $ Binary $ BS.toStrict $ toMixedEndianByteString rid
      d' = toSql $ Datetime2 d
      n' = toSql $ T.pack n
      e' = toSql $ T.pack e
      q' = toSql q
      sql =
        "INSERT INTO [dbo].[Reservations] ([Guid], [Date], [Name], [Email], [Quantity])\
        \VALUES (" <> rid' <> ", " <> d' <> ", " <> n' <> ", " <> e' <> ", " <> q' <> ")"
  in bracket (connect connStr) close (`exec` sql)

newtype UniqueIdentifier = UniqueIdentifier UUID deriving (Eq, Show, Read)

instance FromValue UniqueIdentifier where
  fromValue v =
    fmap (fromMixedEndianByteString . BS.fromStrict . unBinary) (fromValue v)
    >>= maybe (Left ("Expected UUID, but got: " ++ show v)) (Right . coerce)

newtype DbReservation =
  DbReservation { unDbReservation :: Reservation } deriving (Eq, Show, Read)

instance FromRow DbReservation where
  fromRow r =
    let rowToReservation (UniqueIdentifier rid, d, n, e, q) =
          coerce $ Reservation rid d (T.unpack n) (T.unpack e) q
    in rowToReservation <$> fromRow r

readReservation :: Text -> UUID -> IO Reservation
readReservation connStr rid =
  let rid' = toSql $ Binary $ BS.toStrict $ toMixedEndianByteString rid
      sql =
        "SELECT [Guid], [Date], [Name], [Email], [Quantity]\
        \FROM [dbo].[Reservations]\
        \WHERE [Guid] = " <> rid'
  in bracket
      (connect connStr)
      close
      (fmap (unDbReservation . head) . (`query` sql))