{-#LANGUAGE OverloadedStrings #-}
module ReservationSQL where

import Control.Exception.Base (bracket)
import Control.Monad.Except
import Data.Maybe
import Data.Coerce
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Data.Time.LocalTime
import Database.ODBC.SQLServer
import ReservationAPI (Reservation(..), ReservationsInstruction(..))

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

newtype UniqueIdentifier = UniqueIdentifier UUID deriving (Eq, Show, Read)

instance ToSql UniqueIdentifier where
  toSql (UniqueIdentifier ui) =
    toSql $ Binary $ BS.toStrict $ toMixedEndianByteString ui

instance FromValue UniqueIdentifier where
  fromValue v =
    fmap (fromMixedEndianByteString . BS.fromStrict . unBinary) (fromValue v)
    >>= maybe (Left ("Expected UUID, but got: " ++ show v)) (Right . coerce)

insertReservation :: Text -> Reservation -> IO ()
insertReservation connStr (Reservation rid d n e q) =
  let rid' = toSql $ UniqueIdentifier rid
      d' = toSql $ Datetime2 d
      n' = toSql $ T.pack n
      e' = toSql $ T.pack e
      q' = toSql q
      sql =
        "INSERT INTO [dbo].[Reservations] ([Guid], [Date], [Name], [Email], [Quantity])\
        \VALUES (" <> rid' <> ", " <> d' <> ", " <> n' <> ", " <> e' <> ", " <> q' <> ")"
  in withConnection connStr (`exec` sql)

newtype DbReservation =
  DbReservation { unDbReservation :: Reservation } deriving (Eq, Show, Read)

instance FromRow DbReservation where
  fromRow r =
    let rowToReservation (UniqueIdentifier rid, d, n, e, q) =
          coerce $ Reservation rid d (T.unpack n) (T.unpack e) q
    in rowToReservation <$> fromRow r

readReservation :: Text -> UUID -> IO (Maybe Reservation)
readReservation connStr rid =
  let rid' = toSql $ UniqueIdentifier rid
      sql =
        "SELECT [Guid], [Date], [Name], [Email], [Quantity]\
        \FROM [dbo].[Reservations]\
        \WHERE [Guid] = " <> rid'
  in withConnection connStr $ \conn -> do
      rs <- query conn sql
      return $ unDbReservation <$> listToMaybe rs

readReservations :: Text -> LocalTime -> LocalTime -> IO [Reservation]
readReservations connStr lo hi =
  let lo' = toSql $ Datetime2 lo
      hi' = toSql $ Datetime2 hi
      sql =
        "SELECT [Guid], [Date], [Name], [Email], [Quantity]\
        \FROM [dbo].[Reservations]\
        \WHERE " <> lo' <> " <= [Date]\
        \AND [Date] <= " <> hi'
  in withConnection connStr $ \conn -> fmap unDbReservation <$> query conn sql

withConnection :: Text -> (Connection -> IO a) -> IO a
withConnection connStr = bracket (connect connStr) close

runInSQLServer :: MonadIO m => Text -> ReservationsInstruction (m a) -> m a
runInSQLServer connStr (ReadReservation rid next) =
  liftIO (readReservation connStr rid) >>= next
runInSQLServer connStr (ReadReservations lo hi next) =
  liftIO (readReservations connStr lo hi) >>= next
runInSQLServer connStr (CreateReservation r next) =
  liftIO (insertReservation connStr r) >> next