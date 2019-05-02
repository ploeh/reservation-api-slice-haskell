{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module ReservationAPI where

import Data.Char
import Data.UUID
import Data.Aeson
import GHC.Generics
import Servant

data Reservation = Reservation
  { reservationId :: UUID
  , reservationDate :: String
  , reservationName :: String
  , reservationEmail :: String
  , reservationQuantity :: Int
  } deriving (Eq, Show, Generic)

modifyReservationFieldLabel =
  let l = length "reservation"
  in map toLower . drop l

reservationOptions = defaultOptions { fieldLabelModifier = modifyReservationFieldLabel }

instance ToJSON Reservation where
  toEncoding = genericToEncoding reservationOptions

instance FromJSON Reservation where
  parseJSON = genericParseJSON reservationOptions

type ReservationAPI =
       Capture "reservationId" UUID :> Get '[JSON] Reservation
  :<|> ReqBody '[JSON] Reservation :> Post '[JSON] ()

reservation :: UUID -> Reservation
reservation rid = Reservation rid "2019-10-04" "Ploeh" "ploeh@example.com" 3

reservationServer :: Server ReservationAPI
reservationServer = getReservation :<|> postReservation
  where
    getReservation rid = return $ reservation rid
    postReservation _ = return ()