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

createTetris    :: Int        -- Initial speed.
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
          wellCurrentPiece = firstPiece,
          wellNextPiece = secondPiece,
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
    (randomGenerator', firstPiece)   = randomTetromino randomGenerator
    (randomGenerator'', secondPiece) = randomTetromino randomGenerator'

gameLoop :: Tetris -> IO Tetris
gameLoop tetris =
  do
    beforeTime' <- SDL.getTicks
    let beforeTime = fromIntegral beforeTime'
    renderTetris tetris
    tetris' <- handleEvents (updateTetris tetris)
    afterTime' <- SDL.getTicks
    let afterTime = fromIntegral afterTime'
    if not $ gameOver tetris then
      do
        let ellapsedTime = afterTime - beforeTime
        -- TODO since sleeping takes atleast 15 ms this has to be tewaked
        -- Probaåly is not worth sleeping when ellapsedTime close to targetTimePerFrameMS
        when (ellapsedTime < targetTimePerFrameMS) $ SDL.delay $ fromIntegral (targetTimePerFrameMS - ellapsedTime)
        gameLoop tetris
    else
      putStrLn "Game over" >> return tetris

gameOver :: Tetris -> Bool
gameOver tetris = wellFull $ tetrisWell tetris

updateTetris :: Tetris -> Tetris
updateTetris = undefined

handleEvents :: Tetris -> IO Tetris
handleEvents = undefined

handleEvent :: Tetris -> SDL.Event -> Tetris
handleEvent = undefined

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
    renderTetromino (wellNextPiece . tetrisWell $ tetris) (tetrisPreviewSurface tetris)

    -- render score status
    scoreSurface <- TTF.renderTextSolid (tetrisFont tetris) ("Score: " ++ show (tetrisScore tetris)) textColor
    rowsClearedSurface <- TTF.renderTextSolid (tetrisFont tetris) ("Rows cleared: " ++ show (tetrisRowsCleared tetris)) textColor

    -- copy surfaces to window surface
    let Point wellX wellY = wellPosition
    blitWellSurface <- SDL.blitSurface (tetrisWellSurface tetris) Nothing windowSurface $ Just $ SDL.Rect wellX wellY 0 0

    let Point previewX previewY = previewPosition
    blitPreviewSurface <- SDL.blitSurface (tetrisPreviewSurface tetris) Nothing windowSurface $ Just $ SDL.Rect previewX previewY 0 0

    let Point scoreX scoreY = scorePosition
    blitScoreSurface <- SDL.blitSurface scoreSurface Nothing windowSurface $ Just $ SDL.Rect scoreX scoreY 0 0
    blitRowsClearedSurface <- SDL.blitSurface rowsClearedSurface Nothing windowSurface $ Just $ SDL.Rect scoreX (scoreY + 20) 0 0

    -- flip the windowsurface to display it
    void $ SDL.flip windowSurface

renderWell :: Well -> SDL.Surface -> IO ()
renderWell well surface =
  do
    renderTetromino (wellCurrentPiece well) surface
    (sequence_ . map (\block -> renderBlock block surface) . wellSolidBlocks) well

renderTetromino :: Tetromino -> SDL.Surface -> IO ()
renderTetromino tetromino surface =
  sequence_ $ map (\block -> renderBlock (block { blockPosition = blockPosition block `pTranslate` tetrominoPosition tetromino }) surface) $ tetrominoBlocks tetromino

renderBlock :: Block -> SDL.Surface -> IO ()
renderBlock block surface = void $ SDL.fillRect surface (Just $ SDL.Rect x y blockSide blockSide) (blockColor block)
  where
    Point x' y' = blockPosition block
    (x, y) = (x' * blockSide, y' * blockSide)
