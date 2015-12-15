module Well where

import Block
import Tetromino
import Util

import Graphics.UI.SDL (Pixel)
import qualified Graphics.UI.SDL as SDL

data Well = Well {
  wellCurrentPiece  :: Tetromino,
  wellNextPiece     :: Tetromino,
  wellSolidBlocks   :: [Block],
  wellFull          :: Bool
} deriving Show
