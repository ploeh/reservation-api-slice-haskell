{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ReservationAPI where

import Data.Char
import Data.List
import Data.Foldable
import Data.ByteString.Lazy (ByteString)
import Data.UUID
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Functor.Sum
import Data.Aeson
import GHC.Generics
import Control.Monad.Trans.Class
import Control.Monad.Except
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
  | ReadReservations LocalTime ([Reservation] -> next)
  | CreateReservation Reservation next
  deriving Functor

newtype ClockInstruction next = CurrentTime (LocalTime -> next) deriving Functor

type ReservationsProgram = Free (Sum ReservationsInstruction ClockInstruction)

readReservation :: UUID -> ReservationsProgram (Maybe Reservation)
readReservation rid = liftF $ InL $ ReadReservation rid id

readReservations :: LocalTime -> ReservationsProgram [Reservation]
readReservations t = liftF $ InL $ ReadReservations t id

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

-- Not in time 1.8.0.2
addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

removeNonOverlappingReservations :: NominalDiffTime
                                 ->  Reservation
                                 -> [Reservation]
                                 -> [Reservation]
removeNonOverlappingReservations seatingDuration r rs =
  let reservationStartsAt = reservationDate r
      aSeatingDurationBefore =
        addLocalTime (negate seatingDuration) reservationStartsAt
      aSeatingDurationAfter =
        addLocalTime seatingDuration reservationStartsAt
      overlaps x = let t = reservationDate x
                   in aSeatingDurationBefore < t && t < aSeatingDurationAfter
  in filter overlaps rs

canAccommodate :: (Foldable f, Foldable g, Ord a, Num a)
               => f a -> g a -> a -> Bool
canAccommodate resources reservations q =
  let resourceList = sort $ toList resources
      reservationList = sort $ toList reservations
      fits reservation resource = reservation <= resource
      remainingResources = deleteFirstsBy fits resourceList reservationList
  in any (q <=) remainingResources

canAccommodateReservation :: [Table]
                          -> [Reservation]
                          -> Reservation
                          -> Either (APIError ByteString) Reservation
canAccommodateReservation tables reservations r =
  if canAccommodate
      (tableSeats <$> tables)
      (reservationQuantity <$> reservations)
      (reservationQuantity r)
    then Right r
    else Left $ ExecutionError "No table available"
  
newtype Table = Table { tableSeats :: Int } deriving (Eq, Show, Read)

tryAccept :: NominalDiffTime
          -> [Table]
          -> Reservation
          -> ExceptT (APIError ByteString) ReservationsProgram ()
tryAccept seatingDuration tables r = do
  now <- lift currentTime
  _ <- liftEither $ validateReservation now r
  reservations <-
    fmap (removeNonOverlappingReservations seatingDuration r) <$>
    lift $ readReservations $ reservationDate r

  _ <- liftEither $ canAccommodateReservation tables reservations r

  lift $ createReservation r

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
