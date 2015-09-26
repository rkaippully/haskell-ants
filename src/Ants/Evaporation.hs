{-
  Ants simulation - Pheromone evaporation
-}

module Ants.Evaporation (
  -- Behaviors
  evaporation
  ) where

import Ants.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array.IArray


-- evaporation rate
evapRate :: Double
evapRate = 0.99

evapSleepMS :: Int
evapSleepMS = 1000000


-- Causes all pheromones to evaporate a bit
evaporate :: World -> IO ()
evaporate w =
  forM_ (elems w) $
    atomically . updateTVar
      (\c -> c {pheromoneQty = round(fromIntegral(pheromoneQty c) * evapRate)})

evaporation :: World -> IO ()
evaporation w = do
  evaporate w
  threadDelay evapSleepMS
  evaporation w
