module Con.Types.Window (Timestamp, Event, Window, empty, singleton, append, sum, rotate) where

import Prelude hiding (sum)
import qualified Data.Heap as H
import qualified Data.Foldable as F

type Timestamp = Int
type Event = (Timestamp, Double)
type Window = H.MinPrioHeap Timestamp Double

empty :: Window
empty = H.empty

singleton :: Event -> Window
singleton = H.singleton

append :: Window -> Window -> Window
append = H.union 

sum :: Window -> Double
sum = F.foldl' (+) 0.0

rotate :: Timestamp -> Window -> Window
rotate t w = case H.view w of
  Just ((ts, d), xs) -> if ts < t then rotate t xs else w
  Nothing -> w
