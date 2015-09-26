{-
  Ants simulation - UI
-}

module Ants.UI (startUI) where

import Ants.Types
import Ants.Ants
import Ants.Evaporation
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array.IArray
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WX hiding (when)

-- Pixels per world cell
scale :: Int
scale = 7

-- scale factor for pheromone drawing
pheromoneScale :: Double
pheromoneScale = 20.0

-- scale factor for food drawing
foodScale :: Double
foodScale = 30.0

drawSquare :: DC a -> Int -> Int -> Int -> Int -> IO ()
drawSquare dc x y w h =
  polyline dc [ point x y
              , point (x+w) y
              , point (x+w) (y+h)
              , point x (y+h)
              , point x y] []

fillCell :: DC a -> Int -> Int -> Color -> IO ()
fillCell dc x y c = do
  set dc [brushColor := c, brushKind := BrushSolid, penColor := white]
  drawRect dc (rect (point (x*scale) (y*scale)) (sz scale scale)) []

renderAnt :: DC a -> Maybe Ant -> Int -> Int -> IO ()
renderAnt _ Nothing _ _ = return ()
renderAnt dc (Just ant) x y =
  line dc startPoint endPoint [penColor := c]
  where
    c = if hasFood ant then red else black
    (startPoint, endPoint) =
      let
        mid = scale `div` 2
        tl = (x*scale, y*scale)
        tm = (x*scale+mid, y*scale)
        tr = ((x+1)*scale-1, y*scale)
        mr = ((x+1)*scale-1, y*scale+mid)
        br = ((x+1)*scale-1, (y+1)*scale-1)
        bm = (x*scale+mid, (y+1)*scale-1)
        bl = (x*scale, (y+1)*scale-1)
        ml = (x*scale, y*scale+mid)
        toPoint (a, b) = point a b
      in
      case direction ant of
      N  -> (toPoint tm, toPoint bm)
      NE -> (toPoint tr, toPoint bl)
      E  -> (toPoint mr, toPoint ml)
      SE -> (toPoint br, toPoint tl)
      S  -> (toPoint bm, toPoint tm)
      SW -> (toPoint bl, toPoint tr)
      W  -> (toPoint ml, toPoint mr)
      NW -> (toPoint tl, toPoint br)

renderPlace :: DC a -> Cell -> Int -> Int -> IO ()
renderPlace dc cell x y = do
  when (pheromoneQty cell > 0) $
    fillCell dc x y (rgba 0 255 0 (scaled (pheromoneQty cell) pheromoneScale))
  when (foodQty cell > 0) $
    fillCell dc x y (rgba 255 0 0 (scaled (foodQty cell) foodScale))
  renderAnt dc (antInCell cell) x y
  where
    scaled :: Int -> Double -> Int
    scaled v s = min 255 (round ((fromIntegral v/s)*255.0))

paintWorld :: World -> DC a -> Rect -> IO ()
paintWorld world dc _ = do
  set dc [brushColor := white, brushKind := BrushSolid]
  drawRect dc (rectBetween (point scale scale) (point ((dim+1)*scale) ((dim+1)*scale))) []
  -- Draw home
  set dc [penColor := blue]
  drawSquare dc (homeOffset*scale) (homeOffset*scale) (nantsSqrt*scale) (nantsSqrt*scale)
  -- Draw places
  forM_ (assocs world) (\ ((x, y), var) -> do
                            cell <- atomically $ readTVar var
                            renderPlace dc cell x y)
animationSleepMS :: Int
animationSleepMS = 100000

animation :: Panel a -> IO ()
animation p = do
  repaint p
  threadDelay animationSleepMS
  animation p

startUI :: IO ()
startUI = start $ do
  world <- setupWorld
  f <- frameFixed [ text := "Haskell Ants Demo"
                  , clientSize := sz ((dim+2)*scale) ((dim+7)*scale)]
  p <- panel f [ position := point scale scale
               , on paint := paintWorld world]

  -- Start threads
  _ <- forkIO $ animation p
  _ <- forkIO $ evaporation world
  sequence_ [forkIO $ antBehavior (world, x, y) |
               x <- [homeOffset..homeOffset+nantsSqrt-1]
             , y <- [homeOffset..homeOffset+nantsSqrt-1]]

  return ()
