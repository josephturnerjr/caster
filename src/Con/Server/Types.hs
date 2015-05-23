module Con.Server.Types (TWAMap, AggrMap, ServerState(..), newServerState) where

import Control.Concurrent

import Con.Types.TimeWindowAggregate
import Con.Types.Aggregate
import qualified Data.Map as M
import Control.Applicative

type TWAMap = M.Map String TimeWindowAggr
type AggrMap = M.Map String Aggregate

data ServerState = ServerState {
  aggrs :: MVar AggrMap,
  timeWindowAggrs :: MVar TWAMap
}

newServerState :: IO ServerState
newServerState = ServerState <$> (newMVar M.empty) <*> (newMVar M.empty)
