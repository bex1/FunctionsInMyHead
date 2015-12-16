module Well where

import Block
import Colors
import Constants
import Tetromino
import Util

import Data.List ( sort )
import qualified Data.Map as Map
import Graphics.UI.SDL (Pixel)
import qualified Graphics.UI.SDL as SDL
import System.Random

-- The well is the playfield in Tetris.
data Well = Well {
  wellCurrentTetromino  :: Tetromino,
  wellNextTetromino     :: Tetromino,
  wellSolidBlocks       :: [Block],
  wellFull              :: Bool
} deriving Show

-- Retrives the positions of the solid blocks of the Well.
wellSolidBlockPositions :: Well -> [Point]
wellSolidBlockPositions = map blockPosition . wellSolidBlocks

-- Turns a tetromino into solid blocks of the well.
solidifyCurrentTetromino :: Well -> Well
solidifyCurrentTetromino well =
  well
    {
      wellSolidBlocks =
        [Block
          {
            blockPosition = position,
            blockColor = solidBlockColor
          }
        | position <- tetrominoBlockPositions $ wellCurrentTetromino well
        ] ++ wellSolidBlocks well
    }

-- Changes the current tetromino into the preview tetromino and
-- picks a random tetromino as the next preview.
newCurrentTetromino :: StdGen -> Well -> (StdGen, Well)
newCurrentTetromino randomGenerator well =
  (randomGenerator',
   well
     {
       wellCurrentTetromino = nextTetromino,
       wellNextTetromino = newTetromino
     }
  )
  where
    (randomGenerator', newTetromino') = randomTetromino randomGenerator
    newTetromino = newTetromino' { tetrominoPosition = previewTetrominoPosition }
    nextTetromino' = wellNextTetromino well
    nextTetromino = nextTetromino' { tetrominoPosition = wellTetrominoStartPosition }

-- Descends the current teromino and returns the number of cleared rows
-- and the updated well.
-- If the tetromino collides the current tetromino is switched and
-- if that one also collide we lose.
performDescend :: Well -> StdGen -> (StdGen, (Int, Well))
performDescend well randomGenerator =
  if descendedTetromino `tetrominoCollidesWithWell` well then
    -- The descended tetromino collided. We either lose or
    -- clear full rows and get a new current tetromino.
    if newTetromino `tetrominoCollidesWithWell` newTetrominoWell then
      -- We lose, the new tetromino collides.
      (randomGenerator', (rowsCleared, newTetrominoWell { wellFull = True }))
    else
      -- We continue with well cleared of full rows and a new current
      -- tetromino.
      (randomGenerator', (rowsCleared, newTetrominoWell))
  else
    -- The decended tetromino did not collide, hence return the well
    -- with the current tetromino descended one level.
    (randomGenerator', (0, desendedTetrominoWell))
  where
    -- The well where the current tetromino is descended one level.
    desendedTetrominoWell = well { wellCurrentTetromino = descendTetromino $ wellCurrentTetromino well }

    -- The decended tetromino.
    descendedTetromino = wellCurrentTetromino desendedTetrominoWell

    -- The well where the current tetromino is solifified and
    -- the current tetromino is changed to the preview one.
    -- Also a new random preview tetromino is assigned.
    -- Hence the random generator.
    (randomGenerator', newTetrominoWell') = (newCurrentTetromino randomGenerator . solidifyCurrentTetromino) well

    -- Get the new well where the full rows are cleared. Also the amount
    -- of rows cleared is fetched.
    (rowsCleared, newTetrominoWell) = clearFullRows newTetrominoWell'

    -- The new current tetromino in the well cleared of full rows.
    newTetromino = wellCurrentTetromino newTetrominoWell

-- Removes the full rows of the well and moves rows above down.
-- Returns the number of rows removed and the updated well.
clearFullRows :: Well -> (Int, Well)
clearFullRows  well =
  (numberFullRows,
   -- for each full row
   -- remove it
   -- and move rows above down one level
   foldl
     (\well row ->
       well
         {
           wellSolidBlocks =                           -- move rows above down
             map                                       -- on level
               (\block ->
                 if blockRow block < row then
                   moveDown block
                 else
                   block
               )
             $ filter (\block -> blockRow block /= row)  -- remove the full row
             $ wellSolidBlocks well
         }
     )
     well
     fullRowNumbers
  )
  where
    -- function that returns the rownumber of a block
    blockRow :: Block -> Int
    blockRow block = let Point _ row = blockPosition block in row

    -- function that moves a block down one level
    moveDown :: Block -> Block
    moveDown block =
      block
        {
          blockPosition = blockPosition block `pTranslate` Point 0 1
        }

    -- Create a map that maps each row number to the number of blocks in
    -- that row.
    rowCountsMap =
      foldl
        (\rowCountsMap' (Point _ row) -> Map.insertWith (\_ count -> count + 1) row 1 rowCountsMap')
        Map.empty
        $ wellSolidBlockPositions well

    -- Calculate the full rows, i.e. a list of the full rownumbers.
    -- Also get the number of full rows.
    (numberFullRows, fullRowNumbers) =
      let fullRowNumbers' = sort
              $ map fst
              $ filter (\(_, count) -> count == wellWidth)
              $ Map.toList rowCountsMap
      in (length fullRowNumbers', fullRowNumbers')

-- Checks if a tetromino is outside the well dimensions or collides with a solid
-- block.
tetrominoCollidesWithWell :: Tetromino -> Well -> Bool
tetrominoCollidesWithWell tetromino well =
  any (`pointCollidesWithWell` well) $ tetrominoBlockPositions tetromino

-- Checks if a point is outside the well dimensions or collides with a solid
-- block.
pointCollidesWithWell :: Point -> Well-> Bool
pointCollidesWithWell point well =
  point `elem` wellSolidBlockPositions well || point `isOutsideWell` well

-- Checks if a point is outside the well dimensions
isOutsideWell :: Point -> Well -> Bool
isOutsideWell (Point x y) well =
  x < 0 || y < 0 || x >= wellWidth || y >= wellHeight

-- Steers the current tetromino, if possible, to the left or right.
steerCurrentTetromino :: Well -> SteerDirection -> Well
steerCurrentTetromino well direction =
  case trySteerCurrentTetromino well direction of
    Just well' -> well'
    Nothing -> well

-- Try to steer the tetromino left or right.
trySteerCurrentTetromino :: Well -> SteerDirection -> Maybe Well
trySteerCurrentTetromino well direction =
  if movedTetromino `tetrominoCollidesWithWell` well then
    Nothing
  else
    Just $
      well
        {
          wellCurrentTetromino = movedTetromino
        }
  where
    currentTetromino = wellCurrentTetromino well
    movedTetromino =
      currentTetromino
        {
          tetrominoPosition = tetrominoPosition currentTetromino `pTranslate` steerVector direction
        }

-- Rotates the current tetromino, if possible, clockwise or counterclockwise.
rotateCurrentTetromino :: Well -> RotationDirection -> Well
rotateCurrentTetromino well direction =
  case tryRotateCurrentTetromino well direction of
    Just well' -> well'
    Nothing -> well

    -- Try to rotate the tetromino clockwise or counterclockwise.
tryRotateCurrentTetromino :: Well -> RotationDirection -> Maybe Well
tryRotateCurrentTetromino well direction =
  if rotatedTetromino `tetrominoCollidesWithWell` well then
    Nothing
  else
    Just $
      well
        {
          wellCurrentTetromino = rotatedTetromino
        }
  where
    rotatedTetromino = rotateTetromino direction $ wellCurrentTetromino well
