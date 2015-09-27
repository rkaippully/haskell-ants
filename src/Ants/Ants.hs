{-
  Ants simulation - Ant behavior.
-}

module Ants.Ants (
  -- Behaviors
  antBehavior
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
import Data.Array.IArray
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Ants.Types
import System.Random

-- State monad transformer
type StateSTM = StateT StdGen STM

-- A cell location in a world
type CellLoc = (World, Pos, Pos)

-- Sleep timings in microseconds
antSleepMS :: Int
antSleepMS = 100000


-- Returns a position bounded by the world limits. The world wraps around at the
-- edges
bounded :: Pos -> Pos
bounded v = v `mod` dim

-- Get the cell at the specified location
getCell :: CellLoc -> StateSTM (TVar Cell, Cell)
getCell (w, x, y) = do
  cell <- lift $ readTVar $ w!(x, y)
  return (w!(x, y), cell)

-- Returns the location of the cell specified by an offset
-- For example, cellAtLoc loc N will return the cell just ahead of the ant.
-- cellAtLoc loc NE will return the cell NE of the ant and so on.
cellAtLoc :: CellLoc -> Ant -> Direction -> StateSTM CellLoc
cellAtLoc (w, x, y) ant dir = do
  let targetDir =
        toEnum $ (fromEnum (direction ant) + fromEnum dir - fromEnum N) `mod` 8
  return $ case targetDir of
    N  -> (w, x, bounded (y-1))
    NE -> (w, bounded (x+1), bounded (y-1))
    E  -> (w, bounded (x+1), y)
    SE -> (w, bounded (x+1), bounded (y+1))
    S  -> (w, x, bounded (y+1))
    SW -> (w, bounded (x-1), bounded (y+1))
    W  -> (w, bounded (x-1), y)
    NW -> (w, bounded (x-1), bounded (y-1))

-- The cell in the direction the ant is facing
cellAhead :: CellLoc -> Ant -> StateSTM (TVar Cell)
cellAhead loc ant = do
  (w, x, y) <- cellAtLoc loc ant N
  return $ w!(x, y)

-- The cell NW of the direction the ant is facing
cellAheadLeft :: CellLoc -> Ant -> StateSTM (TVar Cell)
cellAheadLeft loc ant = do
  (w, x, y) <- cellAtLoc loc ant NW
  return $ w!(x, y)

-- The cell NE of the direction the ant is facing
cellAheadRight :: CellLoc -> Ant -> StateSTM (TVar Cell)
cellAheadRight loc ant = do
  (w, x, y) <- cellAtLoc loc ant NE
  return $ w!(x, y)

-- Ant takes one food from the current cell
takeFood :: CellLoc -> Ant -> StateSTM ()
takeFood loc ant = do
  (cell, _) <- getCell loc
  lift $ updateTVar (\c -> c{foodQty = foodQty c - 1,
                             antInCell = Just (ant{hasFood = True})}) cell

-- Drop food carried by ant at the current cell
dropFood :: CellLoc -> Ant -> StateSTM ()
dropFood loc ant = do
  (cell, _) <- getCell loc
  lift $ updateTVar (\c -> c{foodQty = foodQty c + 1,
                             antInCell = Just (ant{hasFood = False})}) cell

-- turn ant at a cell by the specified amount
turn :: CellLoc -> Ant -> Int -> StateSTM CellLoc
turn loc ant amt = do
  (var, _) <- getCell loc
  let newDir = toEnum $ (fromEnum(direction ant) + amt) `mod` 8
  lift $ updateTVar (\c -> c{antInCell = Just ant{direction = newDir}}) var
  return loc

-- Move the ant in forward direction. Returns the new location
moveForward :: CellLoc -> Ant -> StateSTM CellLoc
moveForward loc ant = do
  (var, cell) <- getCell loc
  newLoc<- cellAtLoc loc ant N
  (newVar, _) <- getCell newLoc
  -- Move the ant
  lift $ updateTVar (\c -> c{antInCell = Nothing}) var
  lift $ updateTVar (\c -> c{antInCell = Just ant}) newVar
  -- Leave pheromone trail at the old cell
  unless (isHome cell) $
    lift $ updateTVar (\c -> c{pheromoneQty = pheromoneQty c + 1}) var
  return newLoc

-- Returns a map of xs to their 1-based rank when sorted by fn
rankBy :: (Ord v) => (k -> STM v) -> [k] -> StateSTM (Map k Int)
rankBy fn xs = do
  ts <- lift $ mapM makeTuple xs
  let sorted = sortBy (comparing fst) ts
  return $ newMap (map snd sorted) [1..(length xs)]
  where
    makeTuple k = do
      v <- fn k
      return (v, k)

computeRanks :: (Eq k, Ord v) =>
                [k]
             -> (k -> STM v)
             -> (k -> STM v)
             -> StateSTM (Map k Int)
computeRanks places f g = do
  m1 <- rankBy f places
  m2 <- rankBy g places
  return $ unionWith (+) m1 m2

-- Given a list of weights, returns the value picked by a random spin of a
-- roulette wheel with compartments proportional to the weights
wrand :: [Int] -> StateSTM Int
wrand weights = do
  gen <- get
  let (r, newGen) = randomR (0, sum weights - 1) gen
  put newGen
  return $ findCompartmentOf r 0 0
  where
    findCompartmentOf r i total =
      if r < weights!!i + total
      then i
      else findCompartmentOf r (i + 1) (total + weights!!i)

-- Make a move forward, turn NW, or NE based on weight functions
-- f and g. A rank is computed by applying these functions to the
-- current, NW, and NE cells and one of the above three options are picked at
-- random with a probability of those weights.
makeMove :: CellLoc
         -> Ant
         -> (Cell -> Int)
         -> (Cell -> Int)
         -> StateSTM CellLoc
makeMove loc ant f g = do
  ahead <- cellAhead loc ant
  aheadLeft <- cellAheadLeft loc ant
  aheadRight <- cellAheadRight loc ant
  aheadIsEmpty <- lift $ liftM (isNothing . antInCell) $ readTVar ahead
  let f' = lifted f
      g' = lifted g
  ranks <- computeRanks [ahead, aheadLeft, aheadRight] f' g'
  let fns = [moveForward loc ant, turn loc ant (-1), turn loc ant 1]
      -- move ahead, turn NW, or turn NE based on a weighted random
  idx <- if aheadIsEmpty
         then wrand [ranks%ahead, ranks%aheadLeft, ranks%aheadRight]
         else wrand [0, ranks%aheadLeft, ranks%aheadRight]
  fns!!idx
  where
    lifted :: (Cell -> Int) -> TVar Cell -> STM Int
    lifted fn var = do
      cell <- readTVar var
      return $ fn cell

-- Ant with food going home. Returns the new position of the ant
goHome :: CellLoc -> Ant -> StateSTM CellLoc
goHome loc ant = do
  (_, cell) <- getCell loc
  ahead <- cellAhead loc ant >>= lift . readTVar
  let aheadIsEmpty = isNothing (antInCell ahead)
  -- case 1: reached home
  if isHome cell then do
    dropFood loc ant
    turn loc ant 4
  -- case 2: home is just ahead and is empty
  else if isHome ahead && aheadIsEmpty then
    moveForward loc ant
  -- case 3: find home direction using pheromone trail
  else
    makeMove loc ant homeRank pheromoneQty
    where
      homeRank :: Cell -> Int
      homeRank cell = if isHome cell then 1 else 0

forage :: CellLoc -> Ant -> StateSTM CellLoc
forage loc ant = do
  (_, cell) <- getCell loc
  ahead <- cellAhead loc ant >>= lift . readTVar
  let aheadIsEmpty = isNothing (antInCell ahead)
  -- case 1: food found
  if foodQty cell > 0 && not (isHome cell) then do
    takeFood loc ant
    turn loc ant 4
  -- case 2: food is just ahead and that cell is empty
  else if foodQty ahead > 0 && not (isHome ahead) && aheadIsEmpty then
    moveForward loc ant
  -- case 3: pick the best cell ahead
  else
    makeMove loc ant foodQty pheromoneQty

-- Ant behavior. Returns the new position of the ant
behave :: CellLoc -> StdGen -> IO (CellLoc, StdGen)
behave loc gen =
  atomically $ runStateT process gen
  where
    process :: StateT StdGen STM CellLoc
    process = do
      (_, cell) <- getCell loc
      let ant = fromJust $ antInCell cell
      if hasFood ant
        then goHome loc ant
        else forage loc ant

antBehavior :: CellLoc -> StdGen -> IO ()
antBehavior loc gen = do
  (newLoc, newGen) <- behave loc gen
  threadDelay antSleepMS
  antBehavior newLoc newGen
