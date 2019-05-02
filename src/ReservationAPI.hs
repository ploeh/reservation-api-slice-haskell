{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ReservationAPI where

import Data.Char
import Data.Coerce
import Data.UUID
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Aeson
import GHC.Generics
import Servant

newtype ZonedTimeStructEq =
  ZonedTimeStructEq ZonedTime
  deriving (Show, ToJSON, FromJSON)

instance Eq ZonedTimeStructEq where
  (ZonedTimeStructEq (ZonedTime lt1 tz1)) == (ZonedTimeStructEq (ZonedTime lt2 tz2)) =
    lt1 == lt2 && tz1 == tz2

data Reservation = Reservation
  { reservationId :: UUID
  , reservationDate :: ZonedTimeStructEq
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
reservation rid =
  let rd = coerce $ ZonedTime
            (LocalTime (fromGregorian 2019 10 4) (TimeOfDay 18 30 0))
            (hoursToTimeZone 1)
  in Reservation rid rd "Ploeh" "ploeh@example.com" 3

reservationServer :: Server ReservationAPI
reservationServer = getReservation :<|> postReservation
  where
    getReservation rid = return $ reservation rid
    postReservation _ = return ()