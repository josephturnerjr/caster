{-# LANGUAGE RecordWildCards #-}
module Con.Types.TimeWindowAggregate (TimePeriod, TimeWindowAggr, newTWAggr, retrieve, accumTM, mean, variance) where

import qualified Data.Map.Strict as M
import Control.Concurrent.MVar
import Con.Types.TimeWindow
import Control.Concurrent.Timer
import Control.Concurrent.Suspend (msDelay)  
import Control.Applicative

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
retrieve k (TimeWindowAggr {..}) =
  maybe (return 0.0) (fmap value . readMVar) (M.lookup k twMap)

sum' :: TimeWindowAggr -> IO Double
sum' = M.foldl' addMVar (return 0.0) . twMap
  where addMVar a b = (+) <$> a <*> (fmap value $ readMVar b)

mean :: TimeWindowAggr -> IO Double
mean x = (sum' x) >>= \s -> return $ s / (fromIntegral . M.size . twMap $ x)

variance :: TimeWindowAggr -> IO Double
variance agg = M.foldl' f (return 0.0) (twMap agg)
  where m = mean agg
        f a b = do
          a' <- a
          m' <- m
          val <- fmap value $ readMVar b
          return $ a' + (val - m') ** 2

accumTM :: String -> Double -> TimeWindowAggr -> IO TimeWindowAggr
accumTM k v twa@(TimeWindowAggr {..}) = case M.lookup k twMap of
  Just mtw -> do
    accumM v mtw
    return twa
  Nothing -> do
    (mtw, reg') <- registerTimeWindow (tpToWPBFMS timePeriod) registry
    accumM v mtw
    return $ TimeWindowAggr (M.insert k mtw twMap) reg' timePeriod

pushBackAll :: MVar [MTimeWindow] -> IO ()
pushBackAll mv = do
  tws <- takeMVar mv
  mapM_ pushBackOne tws
  putMVar mv tws

pushBackOne :: MTimeWindow -> IO ()
pushBackOne mtw = modifyMVar_ mtw (\x -> return $! pushBack x)

newWindowRegistry :: IO WindowRegistry
newWindowRegistry = return M.empty

newMTimeWindow :: IO MTimeWindow
newMTimeWindow = newMVar newTimeWindow

accumM :: Double -> MTimeWindow -> IO ()
accumM d mtw = modifyMVar_ mtw (return . accum d)

takeAndCons :: MTimeWindow -> MVar [MTimeWindow] -> IO ()
takeAndCons mtw mmtw = modifyMVar_ mmtw (return . (:) mtw)

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
