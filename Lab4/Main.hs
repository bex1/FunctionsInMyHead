module Main where

import Util
import Constants

import System.Random
import qualified Graphics.UI.SDL     as SDL
import qualified Graphics.UI.SDL.TTF as TTF

main = do
       SDL.init [SDL.InitEverything]
       TTF.init
       font <- TTF.openFont fontPath fontSize
       rng <- getStdGen
       TTF.quit
       SDL.quit
