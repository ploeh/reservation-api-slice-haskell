{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module API where

import Control.Monad.Trans.Free
import Data.Text
import Data.Aeson.TH
import Network.Wai.Handler.Warp
import Servant
import ReservationAPI
import qualified ReservationSQL as DB

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type UserAPI = Get '[JSON] [User]

startApp :: Port -> String -> IO ()
startApp port connStr = run port $ app (pack connStr)

app :: Text -> Application
app connStr = serve api $ server connStr

api :: Proxy API
api = Proxy

type API = "users" :> UserAPI :<|> "reservations" :> ReservationAPI

interpretReservations :: Text -> Free ReservationsInstruction a -> IO a
interpretReservations connStr = iterM go
  where go (ReadReservations _ next) = next []
        go (CreateReservation r next) = do DB.insertReservation connStr r; next

server :: Text -> Server API
server connStr = userServer :<|> reservationServer (interpretReservations connStr)

userServer :: Server UserAPI
userServer = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
