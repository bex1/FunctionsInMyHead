
module Lab3 where

  import Test.QuickCheck
  import Data.Char
  import Data.Maybe
  import Data.List

  -------------------------------------------------------------------------

  data Sudoku = Sudoku {rows :: [[Maybe Int]]}
    deriving (Show, Eq)

  allBlankSudoku :: Sudoku
  allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

  validCell :: Maybe Int -> Bool
  validCell Nothing  = True
  validCell (Just value) = value >= 1 && value <= 9

  validCells :: Sudoku -> Bool
  validCells = all validCell . concat . rows

  validDimensions :: Sudoku -> Bool
  validDimensions (Sudoku rows') =
    length rows' == 9 && all (\ row -> length row == 9) rows'

  isSudoku :: Sudoku -> Bool
  isSudoku s = validCells s && validDimensions s

  isSolved :: Sudoku -> Bool
  isSolved (Sudoku rows') = Nothing `notElem` concat rows'

  -------------------------------------------------------------------------

  showCell :: Maybe Int -> String
  showCell Nothing      = "."
  showCell (Just value) = show value

  showRow :: [Maybe Int] -> String
  showRow = foldr ((++) . showCell) ""

  printSudoku :: Sudoku -> IO ()
  printSudoku = putStr . unlines . map showRow . rows

  readSudoku :: FilePath -> IO Sudoku
  readSudoku file =
    do
      contents <- readFile file
      let sudoku = parseSudoku contents
      if isSudoku sudoku
        then return sudoku
        else error $ file ++ "does not contain a valid Sudoku."

  parseSudoku :: String -> Sudoku
  parseSudoku = Sudoku . map parseRow . lines

  parseRow :: String -> [Maybe Int]
  parseRow = map parseCell

  parseCell :: Char -> Maybe Int
  parseCell '.'  = Nothing
  parseCell char = Just $ ord char

  -------------------------------------------------------------------------

  cell :: Gen (Maybe Int)
  cell =
    frequency
      [(9, return Nothing),
       (1, elements $ map Just [1..9])]

  instance Arbitrary Sudoku where
    arbitrary =
      do rows' <- sequence [ sequence [ cell | j <- [1..9]] | i <- [1..9]]
         return $ Sudoku rows'

  prop_Sudoku :: Sudoku -> Bool
  prop_Sudoku = isSudoku

  -------------------------------------------------------------------------

  type Block = [Maybe Int]

  isOkayBlock :: Block -> Bool
  isOkayBlock block = length onlyJust == length (nub onlyJust)
    where
      onlyJust  = catMaybes block

  blocks :: Sudoku -> [Block]
  blocks (Sudoku rows') = rowBlocks ++ columnBlocks ++ squareBlocks
    where
      rowBlocks    = rows'
      columnBlocks = transpose rows'
      squareBlocks = threeByThreeBlocks rows'

  threeByThreeBlocks :: [[Maybe Int]] -> [Block]
  threeByThreeBlocks rows =
      [take 9 $ drop numProcessed ungroupedBlocks | numProcessed <- [0,9..72]]
    where
      ungroupedBlocks =
        concat [take 3 $ drop numTakenInRow (rows !! rowIndex) |
                numTakenInRow <- [0,3,6], rowIndex <- [0..8]]

  prop_blocks :: Sudoku -> Bool
  prop_blocks sudoku =
    length allBlocks == 27 && all (\ block -> length block == 9) allBlocks
    where
      allBlocks = blocks sudoku

  isOkay :: Sudoku -> Bool
  isOkay = all isOkayBlock . blocks

  -------------------------------------------------------------------------
