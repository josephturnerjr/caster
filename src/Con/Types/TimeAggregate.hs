module Con.Types.TimeAggregate (newTimeAgg, retrieve, addEvent) where

import Prelude hiding (sum)
import qualified Data.Map as M
import qualified Con.Types.Window as W

type TimeAggregate = M.Map String W.Window

newTimeAgg :: TimeAggregate
newTimeAgg = M.empty

addEvent :: String -> W.Event -> TimeAggregate -> TimeAggregate
addEvent k e = M.insertWith W.append k (W.singleton e)

retrieve :: String -> TimeAggregate -> Double
retrieve k = W.sum . (M.findWithDefault W.empty k)

sum :: TimeAggregate -> Double
sum = M.foldl' (addWSum) 0.0
  where addWSum d w = d + (W.sum w)

mean :: TimeAggregate -> Double
mean agg = (sum agg) / (fromIntegral . M.size $ agg)

variance :: TimeAggregate -> Double
variance agg = M.foldl' f 0.0 agg
  where m = mean agg
        f a b = a + ((W.sum b) - m) ** 2

rotate :: W.Timestamp -> TimeAggregate -> TimeAggregate
rotate t = M.map (W.rotate t)
