{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module API where

import Control.Monad.Except
import Control.Monad.Free (toFreeT)
import Control.Monad.Trans.Free
import Data.Text
import Servant
import ReservationAPI
import qualified ReservationSQL as DB

api :: Proxy API
api = Proxy

type API = "reservations" :> ReservationAPI

runInSQLServer :: MonadIO m => Text -> FreeT ReservationsInstruction m a -> m a
runInSQLServer connStr = iterT go
  where go (ReadReservation rid next) =
          liftIO (DB.readReservation connStr rid) >>= next
        go (ReadReservations _ _ next) = next []
        go (CreateReservation r next) =
          liftIO (DB.insertReservation connStr r) >> next

reservationServer :: Monad m =>
                     ServerT ReservationAPI (FreeT ReservationsInstruction (ExceptT ServantErr m))
reservationServer = getReservation :<|> postReservation
  where
    getReservation rid = do
      mr <- toFreeT $ readReservation rid
      case mr of
        Just r -> return r
        Nothing -> throwError err404
    postReservation = toFreeT . createReservation

server :: Monad m =>
          ServerT API (FreeT ReservationsInstruction (ExceptT ServantErr m))
server = reservationServer