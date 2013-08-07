module Logger ( initLogger
              , logMessage
              , logStop
              ) where


import System.IO
import Control.Concurrent

data Logger = Logger Handle (Chan LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: Handle -> IO Logger
initLogger hdl = do
  hSetBuffering hdl LineBuffering
  chan <- newChan
  let l = Logger hdl chan
  forkFinally (logger l) (\_ -> hClose hdl)
  return l

logger :: Logger -> IO ()
logger (Logger hdl chan) = loop
  where loop = do
          cmd <- readChan chan
          case cmd of
               Message msg -> do
                 hPutStrLn hdl msg
                 loop
               Stop s -> do
                 hPutStrLn hdl "logger: stop"
                 putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger _ chan) s = writeChan chan (Message s)

logStop :: Logger -> IO ()
logStop (Logger _ chan) = do
  s <- newEmptyMVar
  writeChan chan (Stop s)
