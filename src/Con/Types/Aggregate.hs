module Con.Types.Aggregate (newAgg, retrieve, aggregate, aggSum, mean, variance) where

import qualified Data.Map as M

type Aggregate = M.Map String Double

newAgg :: Aggregate
newAgg = M.empty

retrieve :: String -> Aggregate -> Double
retrieve = M.findWithDefault 0.0

aggregate :: String -> Double -> Aggregate -> Aggregate
aggregate = M.insertWith (+)

aggSum :: Aggregate -> Double
aggSum = M.foldl' (+) 0.0

mean :: Aggregate -> Double
mean agg = (aggSum agg) / (fromIntegral . M.size $ agg)

variance :: Aggregate -> Double
variance agg = M.foldl' f 0.0 agg
  where m = mean agg
        f a b = a + (b - m) ** 2
