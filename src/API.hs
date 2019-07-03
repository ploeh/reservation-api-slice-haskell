{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module API where

import Control.Monad.Except
import Control.Monad.Free (toFreeT)
import Control.Monad.Trans.Free
import Data.Functor.Sum
import Data.Time.Clock
import Servant
import ReservationAPI

api :: Proxy API
api = Proxy

type API = "reservations" :> ReservationAPI

type ReservationsProgramT = FreeT (Sum ReservationsInstruction ClockInstruction)

reservationServer :: NominalDiffTime
                  -> [Table]
                  -> ServerT ReservationAPI (ReservationsProgramT Handler)
reservationServer seatingDuration tables = getReservation :<|> postReservation
  where
    getReservation rid = do
      mr <- toFreeT $ readReservation rid
      case mr of
        Just r -> return r
        Nothing -> throwError err404
    postReservation r = do
      e <- toFreeT $ runExceptT $ tryAccept seatingDuration tables r
      case e of
        Right () -> return ()
        Left (ValidationError err) -> throwError $ err400 { errBody = err }
        Left  (ExecutionError err) -> throwError $ err500 { errBody = err }

server :: NominalDiffTime
       -> [Table]
       -> ServerT API (ReservationsProgramT Handler)
server = reservationServer