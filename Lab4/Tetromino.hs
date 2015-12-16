module Tetromino where

import Block
import Colors
import Util

import Graphics.UI.SDL (Pixel)
import qualified Graphics.UI.SDL as SDL
import System.Random

-- Piece of a tetris game.
data Tetromino = Tetromino
  {
    -- Position is relative to the containing Well.
    tetrominoPosition :: Point,
    tetrominoBlocks   :: [Block]
  } deriving (Show, Eq)

-- Fetches the points of the blocks in a tetromino relative to the Well.
tetrominoBlockPositions :: Tetromino -> [Point]
tetrominoBlockPositions tetromino =
  [ blockPosition block `pTranslate` tetrominoPosition tetromino | block <- tetrominoBlocks tetromino]

-- Descends a tetromino one level in the well.
descendTetromino :: Tetromino -> Tetromino
descendTetromino tetromino =
  tetromino
    {
      tetrominoPosition = Point x (y + 1)
    }
  where
    Point x y = tetrominoPosition tetromino

-- Rotates the tetromino in the specified rotation direction.
rotateTetromino :: RotationDirection -> Tetromino -> Tetromino
rotateTetromino rotationDirection tetromino =
  tetromino
    {
      tetrominoBlocks =
        [block
          {
             blockPosition = rotate90 rotationDirection $ blockPosition block
          }
        | block <- tetrominoBlocks tetromino]
    }

-- Fetches a random tetromino.
randomTetromino :: StdGen -> (StdGen, Tetromino)
randomTetromino randomGenerator = (randomGenerator', tetrominos !! i)
  where
    tetrominos =
      map (\(color', blockPositions) ->
        Tetromino
         {
            tetrominoBlocks = map (\(x, y) ->
              Block
                {
                  blockPosition = Point x y,
                  blockColor = color'
                })
              blockPositions,
            tetrominoPosition = Point 0 0
          })
        [(red, [(-2, -1), (-1, -1), (0, -1), (1, -1)]),
         (green, [(-2, -1), (-1, -1), (0, -1), (0, 0)]),
         (blue, [(-1, -1), (0, -1), (1, -1), (-1, 0)]),
         (yellow, [(-1, -1), (0, -1), (-1, 0), (0, 0)]),
         (cyan, [(0, -1), (1, -1), (-1, 0), (0, 0)]),
         (brown, [(-1, -1), (0, -1), (1, -1), (0, 0)]),
         (orange, [(-1, -1), (0, -1), (0, 0), (1, 0)])]
    (i, randomGenerator') = randomR (0, length tetrominos - 1) randomGenerator
