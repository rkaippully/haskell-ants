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
import Data.Array.IArray
import Data.Hashable
import Data.HashMap.Lazy (HashMap, fromList, unionWith)
import qualified Data.HashMap.Lazy as M
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Ants.Types
import System.Random
import Debug.Trace

-- A cell location in a world
type CellLoc = (World, Pos, Pos)

-- Sleep timings in microseconds
antSleepMS :: Int
antSleepMS = 40000


-- HashMap lookup
(#) :: (Eq k, Hashable k) => HashMap k v -> k -> v
(#) = (M.!)

-- Returns a position bounded by the world limits. The world wraps around at the
-- edges
bounded :: Pos -> Pos
bounded v = v `mod` dim

-- Get the cell and the ant in the cell at the specified location
getCellAndAnt :: CellLoc -> STM (TVar Cell, Cell, Ant)
getCellAndAnt (w, x, y) = do
  cell <- readTVar $ w!(x, y)
  let ant = fromMaybe undefined (antInCell cell)
  return (w!(x, y), cell, ant)

-- Returns the location of the cell specified by an offset
-- For example, cellAtLoc loc N will return the cell just ahead of the ant.
-- cellAtLoc loc NE will return the cell NE of the ant and so on.
cellAtLoc :: CellLoc -> Direction -> STM CellLoc
cellAtLoc loc@(w, x, y) dir = do
  (_, _, ant) <- getCellAndAnt loc
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
cellAhead :: CellLoc -> STM (TVar Cell)
cellAhead loc = do
  (w, x, y) <- cellAtLoc loc N
  return $ w!(x, y)

-- The cell NW of the direction the ant is facing
cellAheadLeft :: CellLoc -> STM (TVar Cell)
cellAheadLeft loc = do
  (w, x, y) <- cellAtLoc loc NW
  return $ w!(x, y)

-- The cell NE of the direction the ant is facing
cellAheadRight :: CellLoc -> STM (TVar Cell)
cellAheadRight loc = do
  (w, x, y) <- cellAtLoc loc NE
  return $ w!(x, y)

-- Ant takes one food from the current cell
takeFood :: CellLoc -> STM ()
takeFood loc = do
  (cell, _, ant) <- getCellAndAnt loc
  updateTVar (\c -> c{foodQty = foodQty c - 1,
                       antInCell = Just (ant{hasFood = True})}) cell

-- Drop food carried by ant at the current cell
dropFood :: CellLoc -> STM ()
dropFood loc = do
  (cell, _, ant) <- getCellAndAnt loc
  updateTVar (\c -> c{foodQty = foodQty c + 1,
                       antInCell = Just (ant{hasFood = False})}) cell

-- turn ant at a cell by the specified amount
turn :: CellLoc -> Int -> STM CellLoc
turn loc amt = do
  (p, _, ant) <- getCellAndAnt loc
  let newDir = toEnum $ (fromEnum(direction ant) + amt) `mod` 8
  updateTVar (\c -> c{antInCell = Just ant{direction = newDir}}) p
  return loc

-- Move the ant in forward direction. Returns the new location
moveForward :: CellLoc -> STM CellLoc
moveForward loc = do
  (p, cell, ant) <- getCellAndAnt loc
  newLoc <- cellAtLoc loc N
  (newP, _, _) <- getCellAndAnt newLoc
  -- Move the ant
  updateTVar (\c -> c{antInCell = Nothing}) p
  updateTVar (\c -> c{antInCell = Just ant}) newP
  -- Leave pheromone trail at the old cell
  unless (isHome cell) $
    updateTVar (\c -> c{pheromoneQty = pheromoneQty c + 1}) p
  return newLoc

-- Returns a map of xs to their 1-based rank when sorted by fn
rankBy :: (Eq k, Hashable k, Ord v) => (k -> v) -> [k] -> HashMap k Int
rankBy fn xs =
  let
    sorted = sortBy (comparing fn) xs
  in
    fromList $ zip sorted [1..(length xs)]

computeRanks :: [Cell] -> (Cell -> Int) -> (Cell -> Int)-> HashMap Cell Int
computeRanks places f g = unionWith (+) (rankBy f places) (rankBy g places)

-- Given a list of values and their weights, returns the value picked by a
-- random spin of a roulette wheel with compartments proportional to the weights
wrand :: StdGen -> [a] -> [Int] -> a
wrand gen vs weights =
  let
    (r, _) = randomR (0, sum weights - 1) gen
  in
    findCompartmentOf r 0 0
  where
    findCompartmentOf r i total =
      if r < weights!!i + total
      then vs!!i
      else findCompartmentOf r (i + 1) (total + weights!!i)

-- Make a move forward, turn NW, or NE based on weight functions
-- f and g. A rank is computed by applying these functions to the
-- current, NW, and NE cells and one of the above three options are picked at
-- random with a probability of those weights.
makeMove :: StdGen -> CellLoc -> (Cell -> Int) -> (Cell -> Int) -> STM CellLoc
makeMove gen loc f g = do
  ahead <- cellAhead loc >>= readTVar
  aheadLeft <- cellAheadLeft loc >>= readTVar
  aheadRight <- cellAheadRight loc >>= readTVar
  let aheadIsEmpty = isNothing (antInCell ahead)
      ranks = computeRanks [ahead, aheadLeft, aheadRight] f g
  trace (show ahead ++ " : " ++ show ranks) $ return ()
  fns <- sequence [moveForward loc, turn loc (-1), turn loc 1]
  -- move ahead, turn NW, or turn NE based on a weighted random number
  return $ if aheadIsEmpty
           then wrand gen fns [ranks#ahead, ranks#aheadLeft, ranks#aheadRight]
           else wrand gen fns [0, ranks#aheadLeft, ranks#aheadRight]

-- Ant with food going home. Returns the new position of the ant
goHome :: StdGen -> CellLoc -> STM CellLoc
goHome gen loc = do
  (_, cell, _) <- getCellAndAnt loc
  ahead <- cellAhead loc >>= readTVar
  let aheadIsEmpty = isNothing (antInCell ahead)
  -- case 1: reached home
  if isHome cell then do
    dropFood loc
    turn loc 4
  -- case 2: home is just ahead and is empty
  else if isHome ahead && aheadIsEmpty then
    moveForward loc
  -- case 3: find home direction using pheromone trail
  else
    makeMove gen loc homeRank pheromoneQty
    where
      homeRank :: Cell -> Int
      homeRank cell = if isHome cell then 1 else 0

forage :: StdGen -> CellLoc -> STM CellLoc
forage gen loc = do
  (_, cell, _) <- getCellAndAnt loc
  ahead <- cellAhead loc >>= readTVar
  let aheadIsEmpty = isNothing (antInCell ahead)
  -- case 1: food found
  if foodQty cell > 0 && not (isHome cell) then do
    takeFood loc
    turn loc 4
  -- case 2: food is just ahead and that cell is empty
  else if foodQty ahead > 0 && not (isHome ahead) && aheadIsEmpty then
    moveForward loc
  -- case 3: pick the best cell ahead
  else
    makeMove gen loc foodQty pheromoneQty

-- Ant behavior. Returns the new position of the ant
behave :: CellLoc -> IO CellLoc
behave loc = do
  randomGen <- getStdGen
  atomically $ do
    (_, _, ant) <- getCellAndAnt loc
    if hasFood ant
    then goHome randomGen loc
    else forage randomGen loc

antBehavior :: CellLoc -> IO ()
antBehavior loc = do
  newLoc <- behave loc
  threadDelay antSleepMS
  antBehavior newLoc
