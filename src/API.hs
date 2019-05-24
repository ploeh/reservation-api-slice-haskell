{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import ReservationAPI
import qualified ReservationSQL as DB

startApp :: Port -> String -> IO ()
startApp port connStr = run port $ app (pack connStr)

app :: Text -> Application
app connStr = serve api $ server connStr

api :: Proxy API
api = Proxy

type API = "reservations" :> ReservationAPI

interpretReservations :: Text -> Free ReservationsInstruction a -> IO a
interpretReservations connStr = iterM go
  where go (ReadReservations _ _ next) = next []
        go (CreateReservation r next) = do DB.insertReservation connStr r; next

reservationServer :: Text -> Server ReservationAPI
reservationServer connStr = getReservation :<|> postReservation
  where
    getReservation rid = return $ readReservation rid
    postReservation = liftIO . interpretReservations connStr . createReservation

server :: Text -> Server API
server connStr = reservationServer connStr
