module Block (Block(..)) where

import Util

import Graphics.UI.SDL (Pixel)

-- A block in tetris.
data Block = Block
  {
    -- Position is relative too the containing tetromino
    -- or relative to the Well if solid.
    blockPosition :: Point,
    blockColor    :: Pixel
  } deriving Show

-- Two blocks are equal if the have the same position. (intersect)
instance Eq Block where
  block1 == block2 = blockPosition block1 == blockPosition block2
