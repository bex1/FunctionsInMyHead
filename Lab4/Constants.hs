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

-- The ppsition of the preview surface.
previewPosition :: Point
previewPosition = wellPosition `pTranslate` Point (wellWidthPixels + blockSide) 0

-- The position of the score.
scorePosition :: Point
scorePosition = wellPosition `pTranslate` Point 0 (-2 * blockSide)

-- The color depth used.
colorDepth :: Int
colorDepth = 24

-- Target is 60 fps. Hence there is approx 1000 / 60
targetTimePerFrameMS :: Int
targetTimePerFrameMS = 1000 `div` 60

-- Initial speed of the game.
initialGameSpeed :: Int
initialGameSpeed = 0

-- Factor to remove from baseFramesPerDescend upon speed increase.
speedIncreaseFactor :: Int
speedIncreaseFactor = 5

-- Speed increase after x number of cleared rows.
speedIncreaseAfterNumberClearedRows :: Int
speedIncreaseAfterNumberClearedRows = 5

-- Descend tetromino every x frames.
baseFramesPerDescend :: Int
baseFramesPerDescend = 30

-- Tetromino startposition in the Well.
wellTetrominoStartPosition :: Point
wellTetrominoStartPosition = Point 5 1

-- The postion of the tetromino in the preview grid.
previewTetrominoPosition :: Point
previewTetrominoPosition = Point 2 3
