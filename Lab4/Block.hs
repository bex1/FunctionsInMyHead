module Block (Block(..)) where

import Util

import Graphics.UI.SDL (Pixel)

data Block = Block
  {
    -- Position is relative too the containing tetromino.
    blockPosition :: Point,
    blockColor    :: Pixel
  } deriving Show
