{-
  Ants simulation - Types
-}

{-# LANGUAGE DeriveGeneric #-}

module Ants.Types (
  -- * Object types
  Pos, Direction(..), Ant(..), Cell(..), World

  -- other constants
  , dim, nantsSqrt, homeOffset

  -- World creation
  , createWorld, setupWorld

  -- Utils
  , updateTVar
  ) where

import System.Random
import Control.Concurrent.STM
import Control.Monad
import Data.Array.IArray
import Data.Hashable
import GHC.Generics (Generic)


data Direction = N | NE | E | SE | S | SW | W | NW
               deriving (Show, Enum, Eq, Ord, Ix, Generic)

instance Hashable Direction

data Ant = Ant {
    direction :: Direction
  , hasFood :: Bool
  } deriving (Show, Eq, Generic)

instance Hashable Ant

data Cell = Cell {
    foodQty :: Int
  , pheromoneQty :: Int
  , antInCell :: Maybe Ant
  , isHome :: Bool
  } deriving (Show, Eq, Generic)

instance Hashable Cell

-- coordinate position of a cell in x or y axis
type Pos = Int

-- World is a 2d vector of tvars to cells
type World = Array (Pos, Pos) (TVar Cell)

-- dimensions of square world
dim :: Pos
dim = 80

-- number of ants = nantsSqrt^2
nantsSqrt :: Int
nantsSqrt = 1

-- number of places with food
foodPlaces :: Int
foodPlaces = 35

-- range of amount of food at a place
foodRange :: Int
foodRange = 100

-- ant home is located here
homeOffset :: Pos
homeOffset = 20


-- Update a TVar by applying a function on the value
updateTVar :: (a -> a) -> TVar a -> STM ()
updateTVar f v = do
  x <- readTVar v
  writeTVar v (f x)


createWorld :: STM World
createWorld = do
  let r = ((1, 1), (dim, dim))
  cells <- mapM createCell (range r)
  return (listArray r cells)
  where
    createCell (x, y) = newTVar Cell {
                          foodQty = 0
                        , pheromoneQty = 0
                        , antInCell = Nothing
                        , isHome = x >= homeOffset
                                && x < homeOffset+nantsSqrt
                                && y >= homeOffset
                                && y < homeOffset+nantsSqrt}

-- Set up the world
setupWorld :: IO World
setupWorld = do
    w <- atomically createWorld
    replicateM_ foodPlaces (placeFood w)
    sequence_ [createAnt w x y | x <- [homeOffset..homeOffset+nantsSqrt-1]
                               , y <- [homeOffset..homeOffset+nantsSqrt-1]]
    return w
  where
    placeFood w = do
      x <- randomRIO (1, dim)
      y <- randomRIO (1, dim)
      qty <- randomRIO (1, foodRange)
      atomically $ updateTVar (\cell -> cell {foodQty = qty}) (w!(x, y))
    createAnt w x y = do
      v <- randomRIO (0, 7)
      let a = Ant {direction = toEnum v, hasFood = False}
      atomically $ updateTVar (\cell -> cell {antInCell = Just a}) (w!(x, y))
