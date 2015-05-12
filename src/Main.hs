import Network
import System.IO
import Control.Concurrent
import Control.Monad
import Text.Printf
import qualified Data.Map.Strict as M

import Con.Server.Commands
import Con.Types
import Con.Server.Types

listenPort :: Int
listenPort = 4242
 
main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral listenPort))
  printf "Listening on port %d\n" listenPort
  st <- newMVar (M.empty :: ValueMap)
  forever $ do
     (handle, host, port) <- accept sock
     printf "Accepted connection from %s: %s\n" host (show port)
     forkIO (runConn handle st) 


runConn :: Handle -> StateMVar -> IO ()
runConn hdl stateMVar = do
  hSetBuffering hdl LineBuffering
  cmdLine <- hGetLine hdl
  resp <- runCmd cmdLine
  hPutStrLn hdl resp
  hClose hdl
