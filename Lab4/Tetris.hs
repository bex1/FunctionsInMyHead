module Tetris where

import Block
import Colors
import Constants
import Tetromino
import Util
import Well

import Control.Monad (when, void, sequence_)
import qualified Graphics.UI.SDL     as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import System.Random

import Debug.Trace

-- NES style Tetris.
data Tetris = Tetris
  {
    tetrisWell            :: Well,
    tetrisScore           :: Int,
    tetrisRowsCleared     :: Int,
    tetrisSpeed           :: Int,
    tetrisFrameNumber     :: Int,
    tetrisRandomGenerator :: StdGen,
    tetrisPreviewSurface  :: SDL.Surface,
    tetrisWellSurface     :: SDL.Surface,
    tetrisFont            :: TTF.Font
  } deriving Show

-- Create a tetris game.
createTetris   :: Int         -- Initial speed.
               -> SDL.Surface -- Well surface.
               -> SDL.Surface -- Preview surface.
               -> TTF.Font    -- Font to use.
               -> StdGen      -- Random generator used in game.
               -> Tetris
createTetris speed wellSurface previewSurface font randomGenerator =
  Tetris
    {
      tetrisWell = Well
        {
          wellCurrentTetromino = firstTetromino,
          wellNextTetromino = secondTetromino,
          wellSolidBlocks = [],
          wellFull = False
        },
      tetrisSpeed = speed,
      tetrisScore = 0,
      tetrisRowsCleared = 0,
      tetrisRandomGenerator = randomGenerator'',
      tetrisFrameNumber = 0,
      tetrisWellSurface = wellSurface,
      tetrisPreviewSurface = previewSurface,
      tetrisFont = font
    }
  where
    (randomGenerator', firstTetromino')   = randomTetromino randomGenerator
    firstTetromino = firstTetromino'
      {
        tetrominoPosition = wellTetrominoStartPosition
      }
    (randomGenerator'', secondTetromino') = randomTetromino randomGenerator'
    secondTetromino = secondTetromino'
      {
        tetrominoPosition = previewTetrominoPosition
      }

-- The loop of the game.
gameLoop :: Tetris -> IO Tetris
gameLoop tetris =
  do
    beforeTime' <- SDL.getTicks
    let beforeTime = fromIntegral beforeTime'
    renderTetris tetris
    tetris' <- handleEvents (updateTetris tetris)
    afterTime' <- SDL.getTicks
    let afterTime = fromIntegral afterTime'
    if not $ gameOver tetris' then
      do
        let ellapsedTime = afterTime - beforeTime
        -- TODO since sleeping takes atleast 15 ms this has to be tewaked
        -- Probaåly is not worth sleeping when ellapsedTime close to targetTimePerFrameMS
        when (ellapsedTime < (targetTimePerFrameMS - 5)) $ SDL.delay $ fromIntegral (targetTimePerFrameMS - ellapsedTime)
        gameLoop tetris'
    else
      putStrLn "Game over" >> return tetris'

-- Indicates if the game is over. I.e the Well is full.
gameOver :: Tetris -> Bool
gameOver tetris = wellFull $ tetrisWell tetris

-- Ends the game.
endGame :: Tetris -> Tetris
endGame tetris =
  tetris
    {
      tetrisWell =
        (tetrisWell tetris)
          {
            wellFull = True
          }
    }

-- Called on every frame to update the game state.
updateTetris :: Tetris -> Tetris
updateTetris tetris
  | tetrisFrameNumber frameUpdatedTetris `mod` (baseFramesPerDescend - tetrisSpeed frameUpdatedTetris) == 0 = tetrominoDescendedTetris
  | otherwise = trace (show $ tetrisFrameNumber frameUpdatedTetris) frameUpdatedTetris -- Only update frame counter
  where
    -- Update frame counter.
    frameUpdatedTetris = tetrisNextFrame tetris

    -- Let current tetromino move down or solidify on collision.
    (randomGenerator, (rowsCleared, well)) = performDescend (tetrisWell frameUpdatedTetris) (tetrisRandomGenerator frameUpdatedTetris)

    -- Calculate game speed. Every 10th cleared row the speed increases
    -- by a factor.
    totalRowsCleared = tetrisRowsCleared frameUpdatedTetris
    speed = (totalRowsCleared `div` speedIncreaseAfterNumberClearedRows) * speedIncreaseFactor

    -- The new tetris with the current tetromino descended one level.
    tetrominoDescendedTetris =
      (updateScore rowsCleared frameUpdatedTetris)
        {
          tetrisSpeed = speed,
          tetrisWell = well,
          tetrisRandomGenerator = randomGenerator
        }

-- Increases the framecount.
tetrisNextFrame :: Tetris -> Tetris
tetrisNextFrame tetris =
  tetris
    {
      tetrisFrameNumber = tetrisFrameNumber tetris + 1
    }

-- Updates the score of tetris.
updateScore :: Int -> Tetris -> Tetris
updateScore rowsCleared tetris =
  tetris
    {
      tetrisScore = tetrisScore tetris + (rowsCleared^2 * 100),
      tetrisRowsCleared = tetrisRowsCleared tetris + rowsCleared
    }

-- Steers the current piece left or right.
steerPiece :: Tetris -> SteerDirection -> Tetris
steerPiece tetris direction =
  tetris
    {
      tetrisWell = steerCurrentTetromino (tetrisWell tetris) direction
    }

-- Rotates the current piece clockwise or counterclockwise.
rotatePiece :: Tetris -> RotationDirection -> Tetris
rotatePiece tetris direction =
  tetris
    {
      tetrisWell = rotateCurrentTetromino (tetrisWell tetris) direction
    }

-- Lets the current piece fall until collision.
-- Then updates the score.
letPieceFall :: Tetris -> Tetris
letPieceFall tetris =
  if descendedSafely then
    letPieceFall
      ((updateScore rowsCleared tetris)
         {
           tetrisWell = well',
           tetrisRandomGenerator = randomGenerator'
         })
  else
    (updateScore rowsCleared tetris)
      {
        tetrisWell = well'
      }
  where
    well = tetrisWell tetris
    descendedSafely =
      ((tetrominoPosition . wellCurrentTetromino) well `pTranslate` Point 0 1)
        == (tetrominoPosition . wellCurrentTetromino) well'
    (randomGenerator', (rowsCleared, well')) = performDescend well randomGenerator
    randomGenerator = tetrisRandomGenerator tetris

-- Polls SDL events until no events remain and handles them.
handleEvents :: Tetris -> IO Tetris
handleEvents tetris =
  do
    event <- SDL.pollEvent
    if event == SDL.NoEvent then
      return tetris
    else
      handleEvents $ handleEvent tetris event

-- Handle a SDL event. Such as keypress etc.
handleEvent :: Tetris -> SDL.Event -> Tetris
handleEvent tetris SDL.NoEvent        = tetris
handleEvent tetris SDL.Quit           = endGame tetris
handleEvent tetris (SDL.KeyDown keysym) =
  case SDL.symKey keysym of
    SDL.SDLK_LEFT   -> steerPiece tetris SteerLeft
    SDL.SDLK_RIGHT  -> steerPiece tetris SteerRight
    SDL.SDLK_DOWN   -> letPieceFall tetris
    SDL.SDLK_z      -> rotatePiece tetris Counterclockwise
    SDL.SDLK_x      -> rotatePiece tetris Clockwise
    SDL.SDLK_q      -> endGame tetris
    SDL.SDLK_ESCAPE -> endGame tetris
    SDL.SDLK_r      -> createTetris
                         initialGameSpeed
                         (tetrisWellSurface tetris)
                         (tetrisPreviewSurface tetris)
                         (tetrisFont tetris)
                         (tetrisRandomGenerator tetris)
    _               -> tetris
handleEvent tetris _ = tetris

-- Renders the game.
renderTetris :: Tetris -> IO ()
renderTetris tetris =
  do
    windowSurface <- SDL.getVideoSurface

    -- clear screen
    SDL.fillRect windowSurface Nothing windowBackgroundColor
    SDL.fillRect (tetrisWellSurface tetris) Nothing wellBackgroundColor
    SDL.fillRect (tetrisPreviewSurface tetris) Nothing wellBackgroundColor

    -- render the Well
    renderWell (tetrisWell tetris) (tetrisWellSurface tetris)

    -- render the tetromino in the preview surface
    renderTetromino (wellNextTetromino . tetrisWell $ tetris) (tetrisPreviewSurface tetris)

    -- render score status
    scoreSurface <- TTF.renderTextSolid (tetrisFont tetris) ("Score: " ++ show (tetrisScore tetris)) textColor
    rowsClearedSurface <- TTF.renderTextSolid (tetrisFont tetris) ("Rows cleared: " ++ show (tetrisRowsCleared tetris)) textColor

    -- copy surfaces to window surface
    let Point wellX wellY = wellPosition
    SDL.blitSurface (tetrisWellSurface tetris) Nothing windowSurface $ Just $ SDL.Rect wellX wellY 0 0

    let Point previewX previewY = previewPosition
    SDL.blitSurface (tetrisPreviewSurface tetris) Nothing windowSurface $ Just $ SDL.Rect previewX previewY 0 0

    let Point scoreX scoreY = scorePosition
    SDL.blitSurface scoreSurface Nothing windowSurface $ Just $ SDL.Rect scoreX scoreY 0 0
    SDL.blitSurface rowsClearedSurface Nothing windowSurface $ Just $ SDL.Rect scoreX (scoreY + 20) 0 0

    -- flip the windowsurface to display it
    void $ SDL.flip windowSurface

-- Renders the Well. I.e. the playfield on a specified Surface.
renderWell :: Well -> SDL.Surface -> IO ()
renderWell well surface =
  do
    renderTetromino (wellCurrentTetromino well) surface
    (sequence_ . map (\block -> renderBlock block surface) . wellSolidBlocks) well

-- Renders a tetromino on the specified SDL surface.
renderTetromino :: Tetromino -> SDL.Surface -> IO ()
renderTetromino tetromino surface =
  sequence_ $ map (\block -> renderBlock (block { blockPosition = blockPosition block `pTranslate` tetrominoPosition tetromino }) surface) $ tetrominoBlocks tetromino

-- Renders a block on the specified Surface.
renderBlock :: Block -> SDL.Surface -> IO ()
renderBlock block surface = void $ SDL.fillRect surface (Just $ SDL.Rect x y blockSide blockSide) (blockColor block)
  where
    Point x' y' = blockPosition block
    (x, y) = (x' * blockSide, y' * blockSide)
