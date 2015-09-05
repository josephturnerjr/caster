{-# LANGUAGE RecordWildCards #-}
module Con.Server.Commands (runCmd) where

import qualified Data.Map.Strict as M
import Control.Concurrent
import Con.Server.Types
import qualified Con.Types.TimeWindowAggregate as TW
import qualified Con.Types.Aggregate as A

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
  -- Point aggregate functions
  ("ACCUM", accumAgg),
  ("GET", getAgg),
  ("MEAN", meanAgg),
  ("VAR", varAgg),
  -- TimeWindow functions
  ("DECLTW", declareTW),
  ("ACCUMTW", accumTW),
  ("GETTW", getTW),
  ("VARTW", varianceTW),
  ("MEANTW", meanTW)
  ]

lookupFn :: String -> Maybe LineCommand
lookupFn name = M.lookup name fnMap

accumAgg :: LineCommand
accumAgg (name:popName:value:[]) (ServerState {..}) =
  modifyMVar aggrs (wrapAgg (accumAgg' name popName value))
accumAgg _ _ = return "ERROR: Wrong number of arguments"

getAgg :: LineCommand
getAgg (name:popName:[]) (ServerState {..})= 
  fmap show $ withMVar aggrs (maybe (return 0.0) (return . A.retrieve popName) . M.lookup name)
getAgg _ _ = return "ERROR: Wrong number of arguments"

meanAgg :: LineCommand
meanAgg (name:[]) (ServerState {..})= 
  fmap show $ withMVar aggrs (maybe (return 0.0) (return . A.mean) . M.lookup name)
meanAgg _ _ = return "ERROR: Wrong number of arguments"

varAgg :: LineCommand
varAgg (name:[]) (ServerState {..})= 
  fmap show $ withMVar aggrs (maybe (return 0.0) (return . A.variance) . M.lookup name)
varAgg _ _ = return "ERROR: Wrong number of arguments"

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

accumAgg' :: String -> String -> String -> AggrMap -> IO (Either AggrMap String)
accumAgg' key popKey valS am = case (readMaybe valS)::Maybe Double of
  Just val -> case (M.lookup key am) of
    Just agg -> do
      return $ Left $ M.insert key (A.aggregate popKey val agg) am
    Nothing -> return $ Left $ M.insert key (A.singleton popKey val) am
  Nothing -> return $ Right "Incorrect value format"

accumTW' :: String -> String -> String -> TWAMap -> IO (Either TWAMap String)
accumTW' key popKey valS twm = case (readMaybe valS)::Maybe Double of
  Just val -> case (M.lookup key twm) of -- TODO replace this with a maybe
    Just twa -> do 
      agg <- TW.accumTM popKey val twa
      return $ Left $ M.insert key agg twm
    Nothing -> return $ Right "Undeclared aggregate name"
  Nothing -> return $ Right "Incorrect value format"

createNewTW :: String -> String -> TWAMap -> IO (Either TWAMap String)
createNewTW name windowLength valueMap = case M.lookup name valueMap of
  Just _ -> return $ Left valueMap
  Nothing -> case (readMaybe windowLength)::Maybe TW.TimePeriod of
    Just len -> do
      agg <- TW.newTWAggr len
      return $ Left $ M.insert name agg valueMap
    Nothing -> return $ Right "Incorrect window length requested"

valFromTW :: String -> String -> TWAMap -> IO Double
valFromTW key popKey = applyToTW key (TW.retrieve popKey) 0.0

meanFromTW :: String -> TWAMap -> IO Double
meanFromTW key = applyToTW key TW.mean 0.0

varianceFromTW :: String -> TWAMap -> IO Double
varianceFromTW key = applyToTW key TW.variance 0.0

applyToTW :: String -> (TW.TimeWindowAggr -> IO a) -> a -> TWAMap -> IO a
applyToTW key f def = maybe (return def) f . M.lookup key

wrapTWMod :: (TWAMap -> IO (Either TWAMap String)) -> TWAMap -> IO (TWAMap, String)
wrapTWMod f m = do
  ret <- f m
  return $ case ret of
             Left m' -> (m', "OK")
             Right err -> (m, err)


wrapAgg :: (AggrMap -> IO (Either AggrMap String)) -> AggrMap -> IO (AggrMap, String)
wrapAgg f m = do
  ret <- f m
  return $ case ret of 
             Left m' -> (m', "OK")
             Right err -> (m, err)
