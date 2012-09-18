module Main where

import UI.TCOD.Console
import UI.TCOD.Console.Types
import UI.TCOD.Color

width = 80
height = 90

main =
  initConsole width height "Color test" defaultConsoleConfig >>=
  drawLoop (cycle colors)

drawLoop :: [Color] -> Console -> IO ()
drawLoop (col:cs) con =
  displayColor con col >>
  flushConsole >> -- Flushes root so don't need argument
  drawLoop cs con

displayColor :: Console -> Color -> IO ()
displayColor con col =
  clear con >>
  setDefaultBackground con col >>
  rect con (0, 0) (width, height) True backSet

names =
  [ Red
  , Flame
  , Orange
  , Amber
  , Yellow
  , Lime
  , Chartreuse
  , Green
  , Sea
  , Turquoise
  , Cyan
  , Sky
  , Azure
  , Blue
  , Han
  , Violet
  , Purple
  , Fuchsia
  , Magenta
  , Pink
  , Crimson
  ]

levels =
  [ Desaturated
  , Lightest
  , Lighter
  , Light
  , Normal
  , Dark
  , Darker
  , Darkest
  ]

combinations = comb' names levels
  where comb' [] _ = []
        comb' (x:xs) [] = comb' xs levels
        comb' xs@(x:_) (y:ys) = (x,y) : comb' xs ys

colors = map f combinations
  where f (name, level) = getColor name level