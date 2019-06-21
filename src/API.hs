{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module API where

import Control.Monad.Except
import Control.Monad.Free (toFreeT)
import Control.Monad.Trans.Free
import Data.Functor.Sum
import Data.Time.LocalTime
import Data.Text
import Servant
import ReservationAPI
import qualified ReservationSQL as DB

api :: Proxy API
api = Proxy

type API = "reservations" :> ReservationAPI

runInSQLServer :: MonadIO m => Text -> ReservationsInstruction (m a) -> m a
runInSQLServer connStr (ReadReservation rid next) =
  liftIO (DB.readReservation connStr rid) >>= next
runInSQLServer       _ (ReadReservations _ _ next) = next []
runInSQLServer connStr (CreateReservation r next) =
  liftIO (DB.insertReservation connStr r) >> next

runOnSystemClock :: MonadIO m => ClockInstruction (m a) -> m a
runOnSystemClock (CurrentTime next) =
  liftIO (zonedTimeToLocalTime <$> getZonedTime) >>= next

runInSQLServerAndOnSystemClock :: MonadIO m
                               => Text
                               -> ReservationsProgramT m a
                               -> m a
runInSQLServerAndOnSystemClock connStr = iterT go
  where go (InL rins) = runInSQLServer connStr rins
        go (InR cins) = runOnSystemClock cins

type ReservationsProgramT = FreeT (Sum ReservationsInstruction ClockInstruction)        

reservationServer :: ServerT ReservationAPI (ReservationsProgramT Handler)
reservationServer = getReservation :<|> postReservation
  where
    getReservation rid = do
      mr <- toFreeT $ readReservation rid
      case mr of
        Just r -> return r
        Nothing -> throwError err404
    postReservation r = do
      e <- toFreeT $ tryAccept r
      case e of
        Right () -> return ()
        Left (ValidationError err) -> throwError $ err400 { errBody = err }
        Left  (ExecutionError err) -> throwError $ err400 { errBody = err }

server :: ServerT ReservationAPI (ReservationsProgramT Handler)
server = reservationServer