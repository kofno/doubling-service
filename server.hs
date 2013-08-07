module Main where

import Logger

import Network
import System.IO
import Text.Printf
import Control.Monad
import Control.Concurrent

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
  where loop = do
          line <- hGetLine h
          if line == "end"
             then hPutStrLn h "Thank you for using the Haskell doubling service"
             else do hPutStrLn h (show (2 * (read line :: Integer)))
          loop

main :: IO ()
main = withSocketsDo $ do
  hLogFile <- openFile "double.log" AppendMode
  logger <- initLogger hLogFile
  sock <- listenOn (PortNumber (fromIntegral port))
  logMessage logger (printf "Listening on port %d" port)
  forever $ do
    (handle, host, port) <- accept sock
    logMessage logger (printf "Accepted connection from %s:%s" host (show port))
    forkFinally (talk handle) (\_ -> hClose handle)

port :: Integer
port = 44444
