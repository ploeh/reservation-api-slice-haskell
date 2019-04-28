module Main where

import System.Environment
import Text.Read
import Data.Maybe
import API

main :: IO ()
main = do
  args <- getArgs
  let port = fromMaybe 8080 $ listToMaybe args >>= readMaybe
  putStrLn $ "Starting server on port " ++ show port ++ "."
  putStrLn "Press Ctrl + C to stop the server."
  startApp port
