{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module API where

import Data.Aeson.TH
import Network.Wai.Handler.Warp
import Servant
import ReservationAPI

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type UserAPI = Get '[JSON] [User]

startApp :: Port -> IO ()
startApp port = run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

type API = "users" :> UserAPI :<|> "reservations" :> ReservationAPI

server :: Server API
server = userServer :<|> reservationServer

userServer :: Server UserAPI
userServer = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
