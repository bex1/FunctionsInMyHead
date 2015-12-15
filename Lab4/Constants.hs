module Constants where

import Util

-- Path to the font used.
fontPath :: String
fontPath = "font.ttf"

-- Size of the font used.
fontSize :: Int
fontSize = 16

-- Window width in pixels.
windowWidth :: Int
windowWidth = 600

-- Window height in pixels.
windowHeight :: Int
windowHeight = 700

-- Block side in pixels.
blockSide :: Int
blockSide = 24

-- Well width in blocks
wellWidth :: Int
wellWidth = 10

-- Well height in blocks.
wellHeight :: Int
wellHeight = 20

-- Well width in pixels.
wellWidthPixels :: Int
wellWidthPixels = wellWidth * blockSide

-- Well height in pixels.
wellHeightPixels :: Int
wellHeightPixels = wellHeight * blockSide

-- Well position.
wellPosition :: Point
wellPosition =
  Point
    (windowWidth  `div` 2 - wellWidthPixels  `div` 2)
    (windowHeight `div` 2 - wellHeightPixels `div` 2)

previewPosition :: Point
previewPosition = wellPosition `pTranslate` Point (wellWidthPixels + blockSide) 0

scorePosition :: Point
scorePosition = wellPosition `pTranslate` Point 0 (-2 * blockSide)

colorDepth :: Int
colorDepth = 24

-- Target is 60 fps. Hence there is approx 1000 / 60
targetTimePerFrameMS :: Int
targetTimePerFrameMS = 1000 `div` 60
