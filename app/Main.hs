module Main where

import System.Environment
import Data.Bifunctor
import Data.Functor.Sum
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Text (Text, pack)
import Control.Monad.Except
import Control.Monad.Trans.Free
import Text.Read
import Network.Wai.Handler.Warp
import Servant
import ReservationAPI
import Paths_RestaurantReservation
import qualified ReservationSQL as DB
import API

main :: IO ()
main = do
  args <- getArgs
  case args of
    [connStr] -> runApp connStr 8080
    [connStr, port] ->
      case readMaybe port of
        Just p -> runApp connStr p
        _ -> putStrLn "Error: Couldn't parse the port number."
    _ -> putStrLn "Usage: RestaurantReservation <connection string> [port]"

runApp :: String -> Int -> IO ()
runApp connStr port = do
  putStrLn $ "Starting server on port " ++ show port ++ "."
  putStrLn "Press Ctrl + C to stop the server."
  let hoistSQL =
        hoistServer api $ runInSQLServerAndOnSystemClock $ pack connStr
  (seatingDuration, tables) <- readConfig
  run port $ serve api $ hoistSQL $ server seatingDuration tables

runOnSystemClock :: MonadIO m => ClockInstruction (m a) -> m a
runOnSystemClock (CurrentTime next) =
  liftIO (zonedTimeToLocalTime <$> getZonedTime) >>= next

runInSQLServerAndOnSystemClock :: MonadIO m
                               => Text
                               -> ReservationsProgramT m a
                               -> m a
runInSQLServerAndOnSystemClock connStr = iterT go
  where go (InL rins) = DB.runInSQLServer connStr rins
        go (InR cins) = runOnSystemClock cins

-- To keep the example simple, the configuration file is simply a tuple Haskell
-- expression, interpreted with `read`. There's no `Read` instance for
-- NominalDiffTime, so the file stores the seating duration as seconds.
-- This design limits the expressivity of the configuration file itself. For
-- example, you can't put comments in the file.
-- A more comprehensive configuration system might introduce something more
-- elaborate, such as Dhall. For this example, however, I think Dhall would be
-- overkill.
readConfig :: IO (NominalDiffTime, [Table])
readConfig = do
  fileName <- getDataFileName "app/Restaurant.config"
  config <- readFile fileName
  return $ first fromInteger $ read config
