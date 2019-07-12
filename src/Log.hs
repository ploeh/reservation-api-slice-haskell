module Log where

import Data.Bifunctor
import Data.Time.Clock
import Data.Time.LocalTime
import Data.UUID
import Text.Read
import ReservationAPI

data LogEntry a b = LogEntry {
    logTime :: UTCTime
  , logOperation :: String
  , logInput :: a
  , logOutput :: b }
  deriving (Eq, Show, Read)

instance Bifunctor LogEntry where
  bimap f g (LogEntry t o inp out) = LogEntry t o (f inp) (g out)

instance Functor (LogEntry a) where
  fmap = bimap id

-- The seemingly redundant Read constraints are to ensure that everythings
-- that's logged can be read back so that a simulation can be run.
writeLogEntry :: (Show a, Read a, Show b, Read b) => String -> a -> b -> IO ()
writeLogEntry operation input output = do
  t <- getCurrentTime
  print $ LogEntry t operation (show input) (show output)

readLogEntry :: (Read a, Read b) => String -> String -> Maybe (LogEntry a b)
readLogEntry name s = do
  (LogEntry t o inp out) <- readMaybe s
  if o == name
    then (LogEntry t o) <$> readMaybe inp <*> readMaybe out
    else Nothing

readReadReservationLogEntry :: String -> Maybe (LogEntry UUID (Maybe Reservation))
readReadReservationLogEntry = readLogEntry "ReadReservation"

readReadReservationsLogEntry :: String -> Maybe (LogEntry (LocalTime, LocalTime) [Reservation])
readReadReservationsLogEntry = readLogEntry "ReadReservations"

readCreateReservationLogEntry :: String -> Maybe (LogEntry Reservation ())
readCreateReservationLogEntry = readLogEntry "CreateReservation"

readCurrentTimeLogEntry :: String -> Maybe (LogEntry () LocalTime)
readCurrentTimeLogEntry = readLogEntry "CurrentTime"