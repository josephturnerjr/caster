module Con.Server.Commands (runCmd) where

import qualified Data.Map.Strict as M
import Control.Concurrent
import Data.Maybe

import Con.Server.Types

type LineCommand = [String] -> StateMVar -> IO String

runCmd :: String -> IO String
runCmd cmdLine = let (cmd:args) = words cmdLine in
  case (lookupFn cmd) of 
    Just fn' -> fn' args stateMVar
    Nothing -> return $ "ERROR: UNKNOWN COMMAND '" ++ cmd ++ "'"

fnMap :: M.Map String LineCommand
fnMap = M.fromList [("GET", getValue), ("SET", setValue), ("KEYS", listKeys)]

lookupFn :: String -> Maybe LineCommand
lookupFn name = M.lookup name fnMap

getValue :: LineCommand
getValue (name:[]) svar = do 
  valueMap <- takeMVar svar
  let resp = fromMaybe "(nil)" (M.lookup name valueMap)
  putMVar svar valueMap
  return resp
getValue _ _ = return "ERROR: Wrong number of arguments"

setValue :: LineCommand
setValue (name:value:[]) svar = do
  valueMap <- takeMVar svar
  let newMap = M.insert name value valueMap
  putMVar svar newMap
  return "OK"
setValue _ _ = return "ERROR: Wrong number of arguments"

listKeys :: LineCommand
listKeys [] svar = do
  valueMap <- takeMVar svar
  let resp = M.keys valueMap
  putMVar svar valueMap
  return (unwords resp)
listKeys _ _ = return "ERROR: Wrong number of arguments"
