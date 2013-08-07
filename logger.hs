module Logger ( initLogger
              , logMessage
              , logStop
              ) where

import System.IO
import Control.Concurrent

data Logger = Logger Handle (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: Handle -> IO Logger
initLogger hdl = do
  hSetBuffering hdl LineBuffering
  m <- newEmptyMVar
  let l = Logger hdl m
  forkFinally (logger l) (\_ -> hClose hdl)
  return l

logger :: Logger -> IO ()
logger (Logger hdl m) = loop
  where loop = do
          cmd <- takeMVar m
          case cmd of
               Message msg -> do
                 hPutStrLn hdl msg
                 loop
               Stop s -> do
                 hPutStrLn hdl "logger: stop"
                 putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger _ m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger _ m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
