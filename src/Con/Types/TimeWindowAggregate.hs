{-# LANGUAGE RecordWildCards #-}
module Con.Types.TimeWindowAggregate (TimePeriod, TimeWindowAggr, newTWAggr, retrieve, accumTM) where

import qualified Data.Map.Strict as M
import Control.Concurrent.MVar
import Con.Types.TimeWindow
import Control.Concurrent.Timer
import Control.Concurrent.Suspend (msDelay)  

type WindowPBFreqMS = Int
type MTimeWindow = MVar TimeWindow
data TimePeriod = Minute | Hour | Day | Week | ThirtyDay | Year deriving (Read, Show)
data TimeWindowAggr = TimeWindowAggr {
  twMap :: M.Map String MTimeWindow,
  registry :: WindowRegistry,
  timePeriod :: TimePeriod
}
type WindowRegistry = M.Map WindowPBFreqMS (MVar [MTimeWindow])

tpToWPBFMS :: TimePeriod -> WindowPBFreqMS
tpToWPBFMS Minute = 600
tpToWPBFMS Hour = 60 * (tpToWPBFMS Minute)
tpToWPBFMS Day = 24 * (tpToWPBFMS Hour)
tpToWPBFMS Week = 7 * (tpToWPBFMS Day)
tpToWPBFMS ThirtyDay = 30 * (tpToWPBFMS Day)
tpToWPBFMS Year = 365 * (tpToWPBFMS Day)

newTWAggr :: TimePeriod -> IO TimeWindowAggr
newTWAggr tp = do
  r <- newWindowRegistry
  return $ TimeWindowAggr M.empty r tp

retrieve :: String -> TimeWindowAggr -> IO Double
retrieve k (TimeWindowAggr {..}) = case M.lookup k twMap of
  Just v -> fmap value (readMVar v)
  Nothing -> return 0.0

accumTM :: String -> Double -> TimeWindowAggr -> IO TimeWindowAggr
accumTM k v twa@(TimeWindowAggr {..}) = case M.lookup k twMap of
  Just mtw -> do
    putStrLn $ "Found value " ++ (show k) ++ " in " ++ (show $ M.keys twMap)
    accumM v mtw
    return twa
  Nothing -> do
    putStrLn $ "No value " ++ (show k) ++ " in " ++ (show $ M.keys twMap)
    (mtw, reg') <- registerTimeWindow (tpToWPBFMS timePeriod) registry
    accumM v mtw
    return $ TimeWindowAggr (M.insert k mtw twMap) reg' timePeriod

pushBackAll :: MVar [MTimeWindow] -> IO ()
pushBackAll mv = do
  putStrLn "doin it"
  tws <- takeMVar mv
  mapM_ pushBackOne tws
  putMVar mv tws

pushBackOne :: MTimeWindow -> IO ()
pushBackOne mtw = do
  tw <- takeMVar mtw
  putStrLn $ show tw
  putMVar mtw $ pushBack tw

newWindowRegistry :: IO WindowRegistry
newWindowRegistry = return M.empty

newMTimeWindow :: IO MTimeWindow
newMTimeWindow = newMVar newTimeWindow

accumM :: Double -> MTimeWindow -> IO ()
accumM d mtw = let accum' d' tw = return $ accum d' tw in
  modifyMVar_ mtw (accum' d)

takeAndCons :: MTimeWindow -> MVar [MTimeWindow] -> IO ()
takeAndCons mtw mmtw = do
  twl <- takeMVar mmtw
  putMVar mmtw (mtw:twl)

startThread :: WindowPBFreqMS -> MTimeWindow -> IO (MVar [MTimeWindow])
startThread f mtw = do
  mmtw <- newMVar [mtw]
  timer <- repeatedTimer (pushBackAll mmtw) (msDelay $ fromIntegral f)
  _ <- repeatedRestart timer
  return mmtw
  
registerTimeWindow :: WindowPBFreqMS -> WindowRegistry -> IO (MTimeWindow, WindowRegistry)
registerTimeWindow f r = do
  mt <- newMTimeWindow
  case M.lookup f r of
    Just ms -> do
      takeAndCons mt ms
      return (mt, r)
    Nothing -> do
      ms <- startThread f mt
      return (mt, M.insert f ms r)

