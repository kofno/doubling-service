module Main where

import Logger

import Network
import System.IO
import Text.Printf
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception

talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan
  race (server h factor c) (receive h c)
  return ()

receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
  line <- hGetLine h
  atomically $ writeTChan c line

server :: Handle -> TVar Integer -> TChan String -> IO ()
server h factor c = do
  f <- atomically $ readTVar factor
  hPrintf h "Current factor: %d\n" f
  loop f
  where
    loop f = do
      action <- atomically $ do
        f' <- readTVar factor
        if (f /= f')
           then return (newfactor f')
           else do
             l <- readTChan c
             return (command f l)
      action

    newfactor f = do
      hPrintf h "new factor: %d\n" f
      loop f

    command f s = case s of
                       "end" ->
                         hPutStrLn h ("Thank you for using the " ++
                                      "Haskell Doubling Service")
                       '*':s -> do
                         case readInteger s of
                              Left err -> hPutStrLn h err
                              Right n  -> atomically $ writeTVar factor n
                         loop f
                       line -> do
                         case readInteger line of
                              Left s  -> hPutStrLn h s
                              Right n -> hPutStrLn h (show (f * n))
                         loop f

readInteger :: String -> Either String Integer
readInteger s = case (reads s :: [(Integer, String)]) of
                     (n, ""):[] -> Right n
                     otherwise  -> Left "Couldn't parse number"

main :: IO ()
main = withSocketsDo $ do
  hLogFile <- openFile "double.log" AppendMode
  logger   <- initLogger hLogFile
  sock     <- listenOn (PortNumber (fromIntegral port))
  factor   <- atomically $ newTVar 2
  logMessage logger (printf "Listening on port %d" port)
  forever $ do
    (handle, host, port) <- accept sock
    logMessage logger (printf "Accepted connection from %s:%s" host (show port))
    forkFinally (talk handle factor) (clientDisconnect logger handle)

clientDisconnect :: Logger -> Handle -> Either SomeException a -> IO ()
clientDisconnect logger _ (Left e) = logMessage logger (printf "Unexpected client disconnect: %s" (show e))
clientDisconnect logger handle _ = hClose handle

port :: Integer
port = 44444
