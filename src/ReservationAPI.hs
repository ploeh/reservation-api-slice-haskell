{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module ReservationAPI where

import Data.Char
import Data.UUID
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Servant

data Reservation = Reservation
  { reservationId :: UUID
  , reservationDate :: LocalTime
  , reservationName :: String
  , reservationEmail :: String
  , reservationQuantity :: Int
  } deriving (Eq, Show, Generic)

data ReservationsInstruction next =
    ReadReservations LocalTime ([Reservation] -> next)
  | CreateReservation Reservation next
  deriving Functor

type ReservationsProgram = Free ReservationsInstruction

readReservations :: LocalTime -> ReservationsProgram [Reservation]
readReservations t = liftF $ ReadReservations t id

createReservation :: Reservation -> ReservationsProgram ()
createReservation r = liftF $ CreateReservation r ()

readReservation :: UUID -> Reservation
readReservation rid =
  let rd = LocalTime (fromGregorian 2019 10 4) (TimeOfDay 18 30 0)
  in Reservation rid rd "Ploeh" "ploeh@example.com" 3

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

reservationServer :: (ReservationsProgram () -> IO ()) -> Server ReservationAPI
reservationServer interpret = getReservation :<|> postReservation
  where
    getReservation rid = return $ readReservation rid
    postReservation = liftIO . interpret . createReservation