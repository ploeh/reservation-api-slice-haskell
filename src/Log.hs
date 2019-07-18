module Log where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import Data.Bifunctor
import Data.Functor.Sum
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
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
    then LogEntry t o <$> readMaybe inp <*> readMaybe out
    else Nothing

readReadReservationLogEntry :: String -> Maybe (LogEntry UUID (Maybe Reservation))
readReadReservationLogEntry = readLogEntry "ReadReservation"

readReadReservationsLogEntry :: String -> Maybe (LogEntry (LocalTime, LocalTime) [Reservation])
readReadReservationsLogEntry = readLogEntry "ReadReservations"

readCreateReservationLogEntry :: String -> Maybe (LogEntry Reservation ())
readCreateReservationLogEntry = readLogEntry "CreateReservation"

readCurrentTimeLogEntry :: String -> Maybe (LogEntry () LocalTime)
readCurrentTimeLogEntry = readLogEntry "CurrentTime"

data ReplayData = ReplayData {
    observationsOfRead :: Map UUID [Maybe Reservation]
  , observationsOfReads :: Map (LocalTime, LocalTime) [[Reservation]]
  , observationsOfCurrentTime :: [LocalTime]
  } deriving (Eq, Show, Read)

readReplayData :: [String] -> ReplayData
readReplayData = foldr tryAddEntry $ ReplayData Map.empty Map.empty []
  where
    tryAddRead l d =
      case readReadReservationLogEntry l of
        Just (LogEntry _ _ rid mr) ->
          let m = observationsOfRead d
              m' = Map.insertWith (++) rid [mr] m
          in d { observationsOfRead = m' }
        Nothing -> d
    tryAddReads l d =
      case readReadReservationsLogEntry l of
        Just (LogEntry _ _ k v) ->
          let m = observationsOfReads d
              m' = Map.insertWith (++) k [v] m
          in d { observationsOfReads = m' }
        Nothing -> d
    tryAddCurrentTime l d =
      case readCurrentTimeLogEntry l of
        Just (LogEntry _ _ _ t) ->
          let ts = observationsOfCurrentTime d
          in d { observationsOfCurrentTime = t:ts }
        Nothing -> d
    tryAddEntry l = tryAddCurrentTime l . tryAddReads l . tryAddRead l

replayReadReservation :: UUID -> State ReplayData (Maybe Reservation)
replayReadReservation rid = do
  m <- State.gets observationsOfRead
  let (observation:rest) = m ! rid
  State.modify (\s -> s { observationsOfRead = Map.insert rid rest m })
  return observation

replayReadReservations :: LocalTime -> LocalTime -> State ReplayData [Reservation]
replayReadReservations lo hi = do
  m <- State.gets observationsOfReads
  let (observation:rest) = m ! (lo, hi)
  State.modify (\s -> s { observationsOfReads = Map.insert (lo, hi) rest m })
  return observation

replayCurrentTime :: State ReplayData LocalTime
replayCurrentTime = do
  xs <- State.gets observationsOfCurrentTime
  let (observation:rest) = xs
  State.modify (\s -> s { observationsOfCurrentTime = rest })
  return observation

replay :: ReplayData -> ExceptT e ReservationsProgram a -> Either e a
replay d = replayImp d . runExceptT
  where
    replayImp :: ReplayData -> ReservationsProgram a -> a
    replayImp rd p = State.evalState (iterM go p) rd
    go (InL (ReadReservation rid next)) = replayReadReservation rid >>= next
    go (InL (ReadReservations lo hi next)) = replayReadReservations lo hi >>= next
    go (InL (CreateReservation _ next)) = next
    go (InR (CurrentTime next)) = replayCurrentTime >>= next