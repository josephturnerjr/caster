{-# LANGUAGE RecordWildCards #-}
module Con.Server.Commands (runCmd) where

import qualified Data.Map.Strict as M
import Control.Concurrent
import Con.Server.Types
import Con.Types.TimeWindowAggregate

type LineCommand = [String] -> ServerState -> IO String

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

runCmd :: String -> ServerState -> IO String
runCmd cmdLine state = let (cmd:args) = words cmdLine in
  case (lookupFn cmd) of 
    Just fn' -> fn' args state
    Nothing -> return $ "ERROR: UNKNOWN COMMAND '" ++ cmd ++ "'"

fnMap :: M.Map String LineCommand
fnMap = M.fromList [
  ("DECLTW", declareTW),
  ("ACCUMTW", accumTW),
  ("GETTW", getTW),
  ("VARTW", varianceTW),
  ("MEANTW", meanTW)
  ]
  --("GET", getValue),
  --("SET", setValue),
  --("KEYS", listKeys)]

lookupFn :: String -> Maybe LineCommand
lookupFn name = M.lookup name fnMap

declareTW :: LineCommand
declareTW (name:windowLength:[]) (ServerState {..}) =
  modifyMVar timeWindowAggrs (wrapTWMod (createNewTW name windowLength))
declareTW _ _ = return "ERROR: Wrong number of arguments"

accumTW :: LineCommand
accumTW (name:popName:value:[]) (ServerState {..}) =
  modifyMVar timeWindowAggrs (wrapTWMod (accumTW' name popName value))
accumTW _ _ = return "ERROR: Wrong number of arguments"

getTW :: LineCommand
getTW (name:popName:[]) (ServerState {..}) =
  fmap show $ withMVar timeWindowAggrs (valFromTW name popName)
getTW _ _ = return "ERROR: Wrong number of arguments"

meanTW :: LineCommand
meanTW (name:[]) (ServerState {..}) =
  fmap show $ withMVar timeWindowAggrs (meanFromTW name)
meanTW _ _ = return "ERROR: Wrong number of arguments"

varianceTW :: LineCommand
varianceTW (name:[]) (ServerState {..}) =
  fmap show $ withMVar timeWindowAggrs (varianceFromTW name) 
varianceTW _ _ = return "ERROR: Wrong number of arguments"

-- Utility functions

accumTW' :: String -> String -> String -> TWAMap -> IO (Either TWAMap String)
accumTW' key popKey valS twm = case (readMaybe valS)::Maybe Double of
  Just val -> case (M.lookup key twm) of -- TODO replace this with a maybe
    Just twa -> do 
      agg <- accumTM popKey val twa
      return $ Left $ M.insert key agg twm
    Nothing -> return $ Right "Undeclared aggregate name"
  Nothing -> return $ Right "Incorrect value format"

createNewTW :: String -> String -> TWAMap -> IO (Either TWAMap String)
createNewTW name windowLength valueMap = case M.lookup name valueMap of
  Just _ -> return $ Left valueMap
  Nothing -> case (readMaybe windowLength)::Maybe TimePeriod of
    Just len -> do
      agg <- newTWAggr len
      return $ Left $ M.insert name agg valueMap
    Nothing -> return $ Right "Incorrect window length requested"

valFromTW :: String -> String -> TWAMap -> IO Double
valFromTW key popKey = applyToTW key (retrieve popKey) 0.0

meanFromTW :: String -> TWAMap -> IO Double
meanFromTW key = applyToTW key mean 0.0

varianceFromTW :: String -> TWAMap -> IO Double
varianceFromTW key = applyToTW key variance 0.0

applyToTW :: String -> (TimeWindowAggr -> IO a) -> a -> TWAMap -> IO a
applyToTW key f def = maybe (return def) f . M.lookup key

wrapTWMod :: (TWAMap -> IO (Either TWAMap String)) -> TWAMap -> IO (TWAMap, String)
wrapTWMod f m = do
  ret <- f m
  return $ case ret of
             Left m' -> (m', "OK") 
             Right err -> (m, err)

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
