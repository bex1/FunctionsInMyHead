
module Lab3 where

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
      if isSudoku sudoku
        then return sudoku
        else error $ file ++ "does not contain a valid Sudoku."

  -- Helper function for readSudoku.
  parseSudoku :: String -> Sudoku
  parseSudoku = Sudoku . map parseRow . lines

  -- Helper function for readSudoku.
  parseRow :: String -> [Maybe Int]
  parseRow = map parseCell

  -- Helper function for readSudoku.
  parseCell :: Char -> Maybe Int
  parseCell '.'  = Nothing
  parseCell char = Just $ ord char

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
