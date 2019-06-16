{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ReservationAPI where

import Data.Char
import Data.ByteString.Lazy (ByteString)
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

validatePositive :: (Ord b, Num b) => a -> b -> Either a b
validatePositive e x = if x > 0 then Right x else Left e

validateReservation :: Reservation -> Either ByteString Reservation
validateReservation (Reservation rid d n e q) = do
  vq <- validatePositive "Quantity must be a positive integer" q
  return $ Reservation rid d n e vq

tryAccept :: Reservation -> ReservationsProgram (Either ByteString ())
tryAccept = traverse createReservation . validateReservation

modifyReservationFieldLabel :: String -> String
modifyReservationFieldLabel =
  let l = length ("reservation" :: String)
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
