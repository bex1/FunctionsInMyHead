
module Main where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List

-------------------------------------------------------------------------

-- A Sudoku is a matrix of cells where a cell can be blank (Nothing) or
-- contain a value (Just value).
data Sudoku = Sudoku {rows :: [[Maybe Int]]}
  deriving (Show, Eq)

-- A Sudoku consisting only of blank cells.
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- Checks if a Sudoko is valid. I.e. alla cells are valid and the
-- dimensions are correct.
isSudoku :: Sudoku -> Bool
isSudoku s = validCells s && validDimensions s

-- Helper function for isSudoku.
validCells :: Sudoku -> Bool
validCells = all validCell . concat . rows

-- Helper function for isSudoku.
validCell :: Maybe Int -> Bool
validCell Nothing  = True
validCell (Just value) = value >= 1 && value <= 9

-- Helper function for isSudoku.
validDimensions :: Sudoku -> Bool
validDimensions (Sudoku rows') =
  length rows' == 9 && all (\ row -> length row == 9) rows'

-- Checks if a Sudoko is solved. I.e. there are no blanks.
isSolved :: Sudoku -> Bool
isSolved (Sudoku rows') = Nothing `notElem` concat rows'

-------------------------------------------------------------------------

-- Prints a Sudoku where a blank is represented by a (.).
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map showRow . rows

-- Helper function for printSudoku.
showRow :: [Maybe Int] -> String
showRow = foldr ((++) . showCell) ""

-- Helper function for printSudoku.
showCell :: Maybe Int -> String
showCell Nothing      = "."
showCell (Just value) = show value

-- Reads and parses a sudoku from a file.
readSudoku :: FilePath -> IO Sudoku
readSudoku file =
  do
    contents <- readFile file
    let sudoku = parseSudoku contents
    if isSudoku sudoku then
      return sudoku
    else
      error $ file ++ " does not contain a valid Sudoku."

-- Helper function for readSudoku.
parseSudoku :: String -> Sudoku
parseSudoku = Sudoku . map parseRow . lines

-- Helper function for readSudoku.
parseRow :: String -> [Maybe Int]
parseRow = map parseCell

-- Helper function for readSudoku.
parseCell :: Char -> Maybe Int
parseCell '.'  = Nothing
parseCell char = Just $ ord char - ord '0'

-------------------------------------------------------------------------

-- Generator defintion for Sudoku.
instance Arbitrary Sudoku where
  arbitrary =
    do rows' <- sequence [ sequence [ cell | j <- [1..9]] | i <- [1..9]]
       return $ Sudoku rows'

-- Generates a cell. Approx 90% will be blanks and 10% values
cell :: Gen (Maybe Int)
cell =
  frequency
    [(9, return Nothing),
     (1, elements $ map Just [1..9])]

-- Test: Check that genereated Sudokus are valid.
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

-- A block is a row, column or a 3x3 square of cells.
type Block = [Maybe Int]

-- Checks that a block is okay. I.e. there are no duplicated non blank
-- cells.
isOkayBlock :: Block -> Bool
isOkayBlock block = length onlyJust == length (nub onlyJust)
  where
    onlyJust  = catMaybes block

-- Extracts all the blocks of a Sudoku.
blocks :: Sudoku -> [Block]
blocks (Sudoku rows') = rowBlocks ++ columnBlocks ++ squareBlocks
  where
    rowBlocks    = rows'
    columnBlocks = transpose rows'
    squareBlocks = threeByThreeBlocks rows'

-- Helper function for blocks.
threeByThreeBlocks :: [[Maybe Int]] -> [Block]
threeByThreeBlocks rows =
    [take 9 $ drop numProcessed ungroupedBlocks | numProcessed <- [0,9..72]]
  where
    ungroupedBlocks =
      concat [take 3 $ drop numTakenInRow (rows !! rowIndex) |
              numTakenInRow <- [0,3,6], rowIndex <- [0..8]]

-- Test: There should be 27 blocks in a Sudoku and each block should
-- be of length 9.
prop_blocks :: Sudoku -> Bool
prop_blocks sudoku =
  length allBlocks == 27 && all (\ block -> length block == 9) allBlocks
  where
    allBlocks = blocks sudoku

-- Checks so that all blocks of a Sudoku are okay.
isOkay :: Sudoku -> Bool
isOkay = all isOkayBlock . blocks

-------------------------------------------------------------------------
-- A cell position
type Pos = (Int, Int)

-- Returns the list of cell positions that are blank in a Sudoku.
blanks :: Sudoku -> [Pos]
blanks sudoku =
  [(i, j) |
   i <- [0..8], j <- [0..8], isNothing $ sudoku `cellAtPosition` (i, j)]

-- Test: All cell positions in the list returned by blanks are blanks in
-- the Sudoku.
prop_blanks :: Sudoku -> Bool
prop_blanks sudoku =
  all (\ position -> isNothing $ sudoku `cellAtPosition` position) $
      blanks sudoku

-- Utility funcion used to fetch a cell in a Sudoku by position.
cellAtPosition :: Sudoku -> Pos -> Maybe Int
cellAtPosition (Sudoku rows') (rowIndex, columnIndex) =
  (rows' !! rowIndex) !! columnIndex

-- Update by index operator.
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (index, newElement) = beforeIndex ++ (newElement : afterIndex)
  where (beforeIndex, _ : afterIndex) = splitAt index list

-- Test: The update by index operator should update the element at specified
-- index.
prop_updateByIndexOperator :: [Int] -> (Int, Int) -> Property
prop_updateByIndexOperator list (index, newElement) = not (null list) ==>
    list !!= (index',newElement) !! index' == newElement
  where
    index' = index `mod` length list

-- Update a sudoku cell by position.
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows') (i, j) newCell =
  Sudoku (rows' !!= (i, (rows' !! i) !!= (j, newCell)))

-- Test: The update function should update the cell at the specified
-- position.
prop_cellUpdated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_cellUpdated sudoku (rowIndex, columnIndex) newCell =
    updatedSudoku `cellAtPosition` (rowIndex', columnIndex') == newCell
  where
    updatedSudoku = update sudoku (rowIndex', columnIndex') newCell
    rowIndex'     = rowIndex `mod` 9
    columnIndex'  = columnIndex `mod` 9

-- Returns the valid candidate numbers that can be inserted in a given
-- position of a Sudoku.
candidates :: Sudoku -> Pos -> [Int]
candidates (Sudoku rows') (rowIndex, columnIndex) =
    rowCandidates `intersect` columnCandidates `intersect` squareCandidates
  where
    rowCandidates     = blockCandidates $ rows' !! rowIndex
    columnCandidates  = blockCandidates $ transpose rows' !! columnIndex
    squareCandidates  =
      blockCandidates $ threeByThreeBlocks rows' !! threeByThreeIndex
    threeByThreeIndex = (rowIndex `div` 3) + (columnIndex `div` 3) * 3

-- Helper function for candidates.
blockCandidates :: Block -> [Int]
blockCandidates block = [1..9] \\ catMaybes block

-- Test: A valid sudoku updated with a canidate value in any position should
-- still be a valid Sudoku.
prop_checkCandidates :: Sudoku -> Pos -> Property
prop_checkCandidates sudoku (rowIndex, columnIndex) = isOkay sudoku ==>
    all isSudoku candidateSudokus && all isOkay candidateSudokus
  where
    rowIndex'        = rowIndex `mod` 9
    columnIndex'     = columnIndex `mod` 9
    candidateValues  = candidates sudoku (rowIndex', columnIndex')
    candidateSudokus =
      [update sudoku (rowIndex', columnIndex') $ Just value
      | value <- candidateValues]

-------------------------------------------------------------------------

-- Tries to solve a Sudoku by recursively trying candidates in blank
-- cells.
solve :: Sudoku -> Maybe Sudoku
solve sudoku
    | not (isSudoku sudoku) || not (isOkay sudoku) = Nothing
    | otherwise = solve' sudoku candidateValues
  where
    blankPosition   = head $ blanks sudoku
    candidateValues = candidates sudoku blankPosition

-- Helper function for solve.
solve' :: Sudoku -> [Int] -> Maybe Sudoku
solve' sudoku candidateValues
    | null candidateValues        = Nothing
    | null $ blanks updatedSudoku = Just updatedSudoku
    | isNothing solution          = solve' sudoku $ tail candidateValues
    | otherwise                   = solution
  where
    blankPosition       = head $ blanks sudoku
    candidate           = head candidateValues
    updatedSudoku       = update sudoku blankPosition (Just candidate)
    nextBlankPosition   = head $ blanks updatedSudoku
    nextCandidateValues = candidates updatedSudoku nextBlankPosition
    solution            = solve' updatedSudoku nextCandidateValues

-- Reads a Sudoku from a given file, solves it, and prints a solution or
-- indicates that none can be found.
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  sudoku <- readSudoku path
  let solution = solve sudoku
  maybe (print "(no solution)") printSudoku solution

-- Given a Sudoku solution and another Sudoku, checks whether the solution
-- is a valid solution of the other Sudoku.
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution sudoku =
  isSudoku solution && isOkay solution && isSolved solution &&
  and [solution `cellAtPosition` filledPosition ==
       sudoku `cellAtPosition` filledPosition |
       filledPosition <- filledPositions sudoku]

-- Helper function for isSolutionOf.
filledPositions :: Sudoku -> [Pos]
filledPositions sudoku =
  [(i, j) |
   i <- [0..8], j <- [0..8], isJust $ sudoku `cellAtPosition` (i, j)]

-- A solution produced by solve is a valid solution.
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku =
    isJust solution ==> fromJust solution `isSolutionOf` sudoku
  where
    solution = solve sudoku

-------------------------------------------------------------------------

main :: IO()
main = do
  quickCheck prop_Sudoku
  quickCheck prop_blocks
  quickCheck prop_blanks
  quickCheck prop_updateByIndexOperator
  quickCheck prop_cellUpdated
  quickCheck prop_checkCandidates
  quickCheck prop_SolveSound
