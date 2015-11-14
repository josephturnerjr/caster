{-# LANGUAGE BangPatterns #-}

import Network
import System.IO
import Control.Concurrent
import Control.Monad
import Text.Printf

import Con.Server.Commands
import Con.Server.Types
import Con.Types.TimeWindow

listenPort :: Int
listenPort = 4242
 
main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral listenPort))
  printf "Listening on port %d\n" listenPort
  st <- newServerState
  forever $ do
     (handle, host, port) <- accept sock
     --printf "Accepted connection from %s: %s\n" host (show port)
     forkIO (runConn handle st) 


runConn :: Handle -> ServerState -> IO ()
runConn hdl state = do
  hSetBuffering hdl LineBuffering
  cmdLine <- hGetLine hdl
  resp <- runCmd cmdLine state
  hPutStrLn hdl resp
  hClose hdl
