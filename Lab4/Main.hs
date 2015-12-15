module Main where

import Util
import Constants
import Tetris

import System.Random
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

main =
  do
    SDL.init [SDL.InitEverything]
    TTF.init
    font <- TTF.openFont fontPath fontSize
    windowSurface <- SDL.setVideoMode windowWidth windowHeight colorDepth [SDL.HWSurface,SDL.DoubleBuf]
    wellSurface <- SDL.createRGBSurface [SDL.HWSurface] (wellWidth * blockSide) (wellHeight * blockSide) colorDepth 0xFF0000 0x00FF00 0x0000FF 0x000000
    previewSurface <- SDL.createRGBSurface [SDL.HWSurface] (5 * blockSide) (5 * blockSide) colorDepth 0xFF0000 0x00FF00 0x0000FF 0x000000
    randomGenerator <- getStdGen
    gameLoop $ createTetris 0 wellSurface previewSurface font randomGenerator
    TTF.quit
    SDL.quit
