module Tests where

import Block
import Colors
import Constants
import Tetromino
import Util
import Well

import System.Random
import Test.QuickCheck
import Test.QuickCheck.All

instance Arbitrary Point where
  arbitrary =
    do
      x <- elements [0..(wellWidth - 1)]
      y <- elements [0..(wellHeight - 1)]
      return $ Point x y

prop_Translate point1@(Point x1 y1) point2@(Point x2 y2) =
  point1 `pTranslate` point2 == Point (x1 + x2) (y1 + y2)

instance Arbitrary RotationDirection where
  arbitrary = elements [Clockwise, Counterclockwise]

prop_Rotate90 point@(Point x y) direction =
  case direction of
    Clockwise -> rotate90 direction point == Point y (-x)
    Counterclockwise -> rotate90 direction point == Point (-y) x

instance Arbitrary SteerDirection where
  arbitrary = elements [SteerLeft, SteerRight]

prop_Steer point@(Point x y) direction =
  case direction of
    SteerLeft -> point `pTranslate` (steerVector direction) == Point (x-1) y
    SteerRight -> point `pTranslate` (steerVector direction) == Point (x+1) y

instance Arbitrary Block where
  arbitrary =
    do
      pos <- arbitrary
      return $ Block { blockPosition = pos, blockColor = red}

instance Arbitrary Tetromino where
  arbitrary =
    do
      pos <- arbitrary
      let tetrominos = map (\(color', blockPositions) ->
                            Tetromino
                             {
                                tetrominoBlocks = map (\(x, y) ->
                                  Block
                                    {
                                      blockPosition = Point x y,
                                      blockColor = color'
                                    })
                                  blockPositions,
                                tetrominoPosition = pos
                              })
                            [(red, [(-2, -1), (-1, -1), (0, -1), (1, -1)]),
                             (green, [(-2, -1), (-1, -1), (0, -1), (0, 0)]),
                             (blue, [(-1, -1), (0, -1), (1, -1), (-1, 0)]),
                             (yellow, [(-1, -1), (0, -1), (-1, 0), (0, 0)]),
                             (cyan, [(0, -1), (1, -1), (-1, 0), (0, 0)]),
                             (brown, [(-1, -1), (0, -1), (1, -1), (0, 0)]),
                             (orange, [(-1, -1), (0, -1), (0, 0), (1, 0)])]
      elements tetrominos

prop_DescendTetromino tetromino =
  descended == tetromino { tetrominoPosition = Point x (y + 1) }
  && tetrominoBlocks descended == tetrominoBlocks tetromino
  where
    descended = descendTetromino tetromino
    Point x y = tetrominoPosition tetromino

prop_RotateTetromino tetromino direction =
    tetrominoPosition tetromino == tetrominoPosition rotated
    && tetrominoBlocks rotated == expectedBlocks
  where
    rotated = rotateTetromino direction tetromino
    expectedBlocks =
      case direction of
        Clockwise ->
          [ block { blockPosition = let Point x y = blockPosition block in Point y (-x) }
          | block <- tetrominoBlocks tetromino]
        Counterclockwise ->
          [ block { blockPosition = let Point x y = blockPosition block in Point (-y) x }
          | block <- tetrominoBlocks tetromino]

prop_TetrominoBlockPositions tetromino =
  tetrominoBlockPositions tetromino == expectedPositions
  where
    expectedPositions =
      [ blockPosition block `pTranslate` tetrominoPosition tetromino
        | block <- tetrominoBlocks tetromino]

instance Arbitrary Well where
  arbitrary =
    do
      next' <- arbitrary
      let next = next' { tetrominoPosition = previewTetrominoPosition }
      size <- elements [0..5]
      solids <- vectorOf size arbitrary
      current <- suchThat arbitrary
        (\tetromino ->
          not
            (any (\pos -> (pos `elem` (map blockPosition solids)))
            $ tetrominoBlockPositions tetromino))
      return
        Well
         {
           wellCurrentTetromino = current,
           wellNextTetromino = next,
           wellSolidBlocks = solids,
           wellFull = False
         }

prop_WellSolidBlockPositions well =
  wellSolidBlockPositions well == (map blockPosition $ wellSolidBlocks well)

prop_SolidifyCurrentTetromino well =
    solidifyCurrentTetromino well == solidifiedWell
  where
    solidifiedWell =
      well { wellSolidBlocks =
        [Block { blockPosition = position, blockColor = solidBlockColor}
         | position <- tetrominoBlockPositions $ wellCurrentTetromino well]
        ++ wellSolidBlocks well}

runTests =
  do
    quickCheck prop_Translate
    quickCheck prop_Rotate90
    quickCheck prop_Steer
    quickCheck prop_DescendTetromino
    quickCheck prop_TetrominoBlockPositions
    quickCheck prop_RotateTetromino
    quickCheck prop_WellSolidBlockPositions
    quickCheck prop_SolidifyCurrentTetromino
