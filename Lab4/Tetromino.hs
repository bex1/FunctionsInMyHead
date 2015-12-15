module Tetromino where

import Block
import Colors
import Util

import Graphics.UI.SDL (Pixel)
import qualified Graphics.UI.SDL as SDL
import System.Random

data Tetromino = Tetromino
  {
    -- Position is relative to the containing Well.
    tetrominoPosition :: Point,
    tetrominoBlocks   :: [Block]
  } deriving Show

tetrominoBlockPositions :: Tetromino -> [Point]
tetrominoBlockPositions tetromino =
  [ blockPosition block `pTranslate` tetrominoPosition tetromino | block <- tetrominoBlocks tetromino]

descendTetromino :: Tetromino -> Tetromino
descendTetromino tetromino =
  tetromino
    {
      tetrominoPosition = Point x (y + 1)
    }
  where
    Point x y = tetrominoPosition tetromino

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
        [(red, [(0, 1), (1, 1), (2, 1), (3, 1)]),
         (green, [(0, 0), (1, 0), (2, 0), (2, 1)]),
         (blue, [(0, 0), (1, 0), (2, 0), (0, 1)]),
         (yellow, [(0, 0), (1, 0), (0, 1), (1, 1)]),
         (cyan, [(1, 0), (2, 0), (0, 1), (1, 1)]),
         (brown, [(0, 0), (1, 0), (2, 0), (1, 1)]),
         (orange, [(0, 0), (1, 0), (1, 1), (2, 1)])]
    (i, randomGenerator') = randomR (0, length tetrominos - 1) randomGenerator
