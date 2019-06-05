{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module ReservationAPI where

import Data.Char
import Data.UUID
import Data.Time.LocalTime
import Data.Aeson
import GHC.Generics
import Control.Monad.Free
import Servant

data Reservation = Reservation
  { reservationId :: UUID
  , reservationDate :: LocalTime
  , reservationName :: String
  , reservationEmail :: String
  , reservationQuantity :: Int
  } deriving (Eq, Show, Read, Generic)

data ReservationsInstruction next =
    ReadReservation UUID (Maybe Reservation -> next)
  | ReadReservations LocalTime LocalTime ([Reservation] -> next)
  | CreateReservation Reservation next
  deriving Functor

type ReservationsProgram = Free ReservationsInstruction

readReservation :: UUID -> ReservationsProgram (Maybe Reservation)
readReservation rid = liftF $ ReadReservation rid id

readReservations :: LocalTime -> LocalTime -> ReservationsProgram [Reservation]
readReservations lo hi = liftF $ ReadReservations lo hi id

createReservation :: Reservation -> ReservationsProgram ()
createReservation r = liftF $ CreateReservation r ()

modifyReservationFieldLabel :: String -> String
modifyReservationFieldLabel =
  let l = length "reservation"
  in map toLower . drop l

reservationOptions :: Options
reservationOptions = defaultOptions { fieldLabelModifier = modifyReservationFieldLabel }

instance ToJSON Reservation where
  toEncoding = genericToEncoding reservationOptions

instance FromJSON Reservation where
  parseJSON = genericParseJSON reservationOptions

type ReservationAPI =
       Capture "reservationId" UUID :> Get '[JSON] Reservation
  :<|> ReqBody '[JSON] Reservation :> Post '[JSON] ()
