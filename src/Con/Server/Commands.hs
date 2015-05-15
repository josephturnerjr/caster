{-# LANGUAGE RecordWildCards #-}
module Con.Server.Commands (runCmd) where

import qualified Data.Map.Strict as M
import Control.Concurrent
import Con.Server.Types
import Con.Types.TimeWindowAggregate

type LineCommand = [String] -> ServerState -> IO String

runCmd :: String -> ServerState -> IO String
runCmd cmdLine state = let (cmd:args) = words cmdLine in
  case (lookupFn cmd) of 
    Just fn' -> fn' args state
    Nothing -> return $ "ERROR: UNKNOWN COMMAND '" ++ cmd ++ "'"

fnMap :: M.Map String LineCommand
fnMap = M.fromList [
  ("GETTW", getTW),
  ("ACCUMTW", accumTW),
  ("DECLTW", declareTW)
  ]
  --("GET", getValue),
  --("SET", setValue),
  --("KEYS", listKeys)]

lookupFn :: String -> Maybe LineCommand
lookupFn name = M.lookup name fnMap

createNewTW :: String -> String -> TWAMap -> IO (Either TWAMap String)
createNewTW name windowLength valueMap = case M.lookup name valueMap of
  Just _ -> return $ Left valueMap
  Nothing -> case (readMaybe windowLength)::Maybe TimePeriod of
    Just len -> do
      agg <- newTWAggr len
      return $ Left $ M.insert name agg valueMap
    Nothing -> return $ Right "Incorrect window length requested"

declareTW :: LineCommand
declareTW (name:windowLength:[]) (ServerState {..}) = do 
  valueMap <- takeMVar timeWindowAggrs
  ret <- createNewTW name windowLength valueMap
  let (resp, newMap) = case ret of
                       Left valueMap' -> ("OK", valueMap') 
                       Right err -> (err, valueMap)
  putMVar timeWindowAggrs newMap
  return resp
declareTW _ _ = return "ERROR: Wrong number of arguments"

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

getFromTW :: String -> String -> TWAMap -> IO Double
getFromTW key popKey = maybe (return 0.0) (retrieve popKey) . M.lookup key

getTW :: LineCommand
getTW (name:popName:[]) (ServerState {..}) = do 
  valueMap <- takeMVar timeWindowAggrs -- TODO replace these with mvar functions
  val <- getFromTW name popName valueMap
  putMVar timeWindowAggrs valueMap
  return (show val)
getTW _ _ = return "ERROR: Wrong number of arguments"

accumTW' :: String -> String -> String -> TWAMap -> IO (Either TWAMap String)
accumTW' key popKey valS twm = case (readMaybe valS)::Maybe Double of
  Just val -> case (M.lookup key twm) of -- TODO replace this with a maybe
    Just twa -> do 
      agg <- accumTM popKey val twa
      return $ Left $ M.insert key agg twm
    Nothing -> return $ Right "Undeclared aggregate name"
  Nothing -> return $ Right "Incorrect value format"

accumTW :: LineCommand
accumTW (name:popName:value:[]) (ServerState {..}) = do 
  valueMap <- takeMVar timeWindowAggrs
  ret <- accumTW' name popName value valueMap
  let (resp, newMap) = case ret of
                       Left valueMap' -> ("OK", valueMap') 
                       Right err -> (err, valueMap)
  putMVar timeWindowAggrs newMap
  return resp
accumTW _ _ = return "ERROR: Wrong number of arguments"

{-
getValue :: LineCommand
getValue (name:[]) ServerState {..} = do 
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
-}
