module Con.Server.Types (TWAMap, ServerState(..), newServerState) where

import Control.Concurrent

import Con.Types.TimeWindowAggregate
import qualified Data.Map as M

type TWAMap = M.Map String TimeWindowAggr

data ServerState = ServerState {
  timeWindowAggrs :: MVar TWAMap
}

newServerState :: IO ServerState
newServerState = fmap ServerState $ newMVar M.empty
