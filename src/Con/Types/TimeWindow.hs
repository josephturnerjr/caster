{-# LANGUAGE RecordWildCards #-}
module Con.Types.TimeWindow (TimeWindow, newTimeWindow, accum, value, pushBack) where

import Data.Sequence as S
import Data.Foldable as F

data TimeWindow = TimeWindow {
  window :: S.Seq Double,
  accumulator :: Double
} deriving Show
  

defaultWindowLength :: Int
defaultWindowLength = 10 

initWindow :: Int -> S.Seq Double
initWindow windowLength = S.fromList $ Prelude.replicate windowLength 0.0

newTimeWindow :: TimeWindow
newTimeWindow = TimeWindow (initWindow defaultWindowLength) 0.0

accum :: Double -> TimeWindow -> TimeWindow
accum d (TimeWindow {..}) = TimeWindow window (accumulator + d)

value :: TimeWindow -> Double
value (TimeWindow {..}) = accumulator + (F.foldl' (+) 0.0 window)

pushBack :: TimeWindow -> TimeWindow
pushBack (TimeWindow {..}) = TimeWindow ((S.drop 1 window) |> accumulator) 0.0
