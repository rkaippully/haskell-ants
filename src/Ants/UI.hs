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
import Control.Monad.Trans
import Data.Array.IArray
import Data.Word
import System.Random
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

-- Pixels per world cell
scale :: Int
scale = 7

-- scale factor for pheromone drawing
pheromoneScale :: Double
pheromoneScale = 20.0

-- scale factor for food drawing
foodScale :: Double
foodScale = 30.0

white :: Color
white = Color 65535 65535 65535

black :: Color
black = Color 0 0 0

blue :: Color
blue = Color 0 0 65535

green :: Color
green = Color 0 65535 0

red :: Color
red = Color 65535 0 0

fillCell :: DrawWindow -> Int -> Int -> Color -> IO ()
fillCell dw x y c = do
  gc <- gcNewWithValues dw newGCValues{foreground = c, background = white}
  drawRectangle dw gc True (x*scale) (y*scale) scale scale

renderAnt :: DrawWindow -> GC -> Maybe Ant -> Int -> Int -> IO ()
renderAnt _ _ Nothing _ _ = return ()
renderAnt dw gc (Just ant) x y = do
  gcSetValues gc newGCValues{foreground = c, background = white}
  drawLine dw gc startPoint endPoint
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
      in
      case direction ant of
      N  -> (tm, bm)
      NE -> (tr, bl)
      E  -> (mr, ml)
      SE -> (br, tl)
      S  -> (bm, tm)
      SW -> (bl, tr)
      W  -> (ml, mr)
      NW -> (tl, br)

scaled :: Color -> Int -> Double -> Color
scaled (Color r g b) v s = Color (trans r) (trans g) (trans b)
  where
    trans :: Word16 -> Word16
    trans x = min 65535 (round (fromIntegral x * s / fromIntegral v))

renderPlace :: DrawWindow -> GC -> Cell -> Int -> Int -> IO ()
renderPlace dw gc cell x y = do
  when (pheromoneQty cell > 0) $
    fillCell dw x y (scaled green (pheromoneQty cell) pheromoneScale)
  when (foodQty cell > 0) $
    fillCell dw x y (scaled red (foodQty cell) foodScale)
  renderAnt dw gc (antInCell cell) x y

paintWorld :: World -> DrawWindow -> IO ()
paintWorld world dw = do
  drawWindowClear dw
  gc <- gcNewWithValues dw newGCValues{foreground = white}
  drawRectangle dw gc True scale scale (dim*scale) (dim*scale)
  -- Draw places
  forM_ (assocs world) (\((x, y), var) -> do
                            cell <- readTVarIO var
                            renderPlace dw gc cell x y)
  -- Draw home
  gcSetValues gc newGCValues{foreground = blue}
  drawRectangle dw gc False (homeOffset*scale) (homeOffset*scale)
    (nantsSqrt*scale) (nantsSqrt*scale)

animationSleepMS :: Int
animationSleepMS = 100000

animation :: DrawingArea -> IO ()
animation da = do
  postGUISync $ widgetQueueDraw da
  threadDelay animationSleepMS
  animation da

startUI :: IO ()
startUI = do
  initGUI
  world <- setupWorld
  window <- windowNew
  set window [windowTitle := "Haskell Ants Demo"]
  window `on` deleteEvent $ liftIO mainQuit >> return False

  da <- drawingAreaNew
  set da [widgetWidthRequest := (dim + 2)*scale
         , widgetHeightRequest := (dim + 2)*scale]
  window `containerAdd` da
  da `on` exposeEvent $ do
    dw <- eventWindow
    lift $ paintWorld world dw
    return True

  widgetShowAll window

  -- Start threads
  _ <- forkIO $ animation da
  _ <- forkIO $ evaporation world
  gen <- newStdGen
  sequence_ [forkIO $ antBehavior (world, x, y) gen |
               x <- [homeOffset..homeOffset+nantsSqrt-1]
             , y <- [homeOffset..homeOffset+nantsSqrt-1]]

  mainGUI
  return ()
