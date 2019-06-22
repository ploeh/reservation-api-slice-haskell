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
import Data.Functor.Sum
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

newtype ClockInstruction next = CurrentTime (LocalTime -> next) deriving Functor

type ReservationsProgram = Free (Sum ReservationsInstruction ClockInstruction)

readReservation :: UUID -> ReservationsProgram (Maybe Reservation)
readReservation rid = liftF $ InL $ ReadReservation rid id

readReservations :: LocalTime -> LocalTime -> ReservationsProgram [Reservation]
readReservations lo hi = liftF $ InL $ ReadReservations lo hi id

createReservation :: Reservation -> ReservationsProgram ()
createReservation r = liftF $ InL $ CreateReservation r ()

currentTime :: ReservationsProgram LocalTime
currentTime = liftF $ InR $ CurrentTime id

validateNotEqual :: Eq a => e -> a -> a -> Either e a
validateNotEqual e x y = if x /= y then Right y else Left e

validatePositive :: (Ord b, Num b) => a -> b -> Either a b
validatePositive e x = if x > 0 then Right x else Left e

validateLessThan :: Ord a => e -> a -> a -> Either e a
validateLessThan e x y = if x < y then Right y else Left e

data APIError a =
    ValidationError a
  | ExecutionError a
  deriving (Eq, Show, Read, Functor)

validateReservation :: LocalTime
                    -> Reservation
                    -> Either (APIError ByteString) Reservation
validateReservation now (Reservation rid d n e q) = do
  vid <- validateNotEqual (ValidationError "ID can't be the nil UUID") nil rid
  vd <- validateLessThan
          (ValidationError "Reservation time must be in the future") now d
  vq <- validatePositive
          (ValidationError "Quantity must be a positive integer") q
  return $ Reservation vid vd n e vq

tryAccept :: Reservation
          -> ReservationsProgram (Either (APIError ByteString) ())
tryAccept r = do
  now <- currentTime
  traverse createReservation $ validateReservation now r

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
