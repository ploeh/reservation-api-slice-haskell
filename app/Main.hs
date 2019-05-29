module Main where

import System.Environment
import Text.Read
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
  startApp port connStr
