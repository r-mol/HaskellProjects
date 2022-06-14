{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Project where

import CodeWorld
import Data.List
import Data.Maybe

-- | A mark for tic-tac-toe.
data Mark = X | O deriving (Eq, Show)

-- | A cell is either empty or has a mark.
type Cell = Maybe Mark

-- | A board is a 2D grid of cells.
type Board = [[Cell]]

-- | Sample 5x4 board.
sampleBoard :: Board
sampleBoard = [ [ x, o, n, o, x ],
                [ n, o, o, x, o ],
                [ x, n, n, n, n ],
                [ n, o, n, x, x ] ] 
                where
                  (o, x, n) = (Just O, Just X, Nothing) 

-- | Initialise an empty NxM board.
initBoard :: (Int, Int) -> Board
initBoard (n, m) = replicate m (replicate n Nothing)

-- | Draw a single mark (X or O).
drawMark :: Mark -> Picture
drawMark O = thickCircle 0.2 0.3
drawMark X = scaled 0.4 0.4 (rotated (pi/4) cross)
  where
    cross = (solidRectangle 0.5 2 <> solidRectangle 2 0.5)

-- | Draw one board cell at given coordinates.
drawCellAt :: Cell -> (Int, Int) -> Picture
drawCellAt cell (i, j) = translated x y (rectangle 1 1 <> cellPicture)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture = case cell of
        Nothing -> blank
        Just m  -> drawMark m

-- | Draw a rectangular board.
drawBoard :: Board -> Picture
drawBoard board
  | winLine /= Nothing = picturesOfWin <> mainPicture
  | otherwise = mainPicture
    where
      mainPicture = picturesOfX <> picturesOfO <> picturesOfN
      
      winLine = winner board
      winTuple = fromJust winLine
      
      picturesOfX = pictures (picturesOf (Just X) (positionsOf (Just X)))
      picturesOfO = pictures (picturesOf (Just O) (positionsOf (Just O)))
      picturesOfN = pictures (picturesOf Nothing (positionsOf Nothing))
      picturesOfWin = colored red(pictures (picturesOf (Just (fst winTuple)) (snd winTuple)))
      
      picturesOf cell cells = map (drawCellAt cell) cells
      positionsOf cell = concat(map (\(xs,y) -> zip (xs) (replicateOfYs xs y)) (coordinatesBoard cell))
      coordinatesBoard cell = (zip (map (positions cell) board) [0,-1..])
      replicateOfYs xs y = (replicate (length xs) y)
 
-- | Find the positions of element in a list.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x==x']

-- | Run a tic-tac-toe game with a sample starting board.
ticTacToe :: Board -> IO ()
ticTacToe board = activityOf board handleGame drawBoard
  where
    height = length board
    width  = length (head board)
    
    -- | Handle mouse clicks to put marks.
    handleGame :: Event -> Board -> Board
    handleGame (PointerPress mouse) 
      | availCoords (pointToCoords mouse) = putMarkAt (pointToCoords mouse)
      | otherwise = id
    handleGame _ = id

    -- | Convert mouse position into board coordinates.
    pointToCoords :: Point -> (Int, Int)
    pointToCoords (x, y) = (round x, round y)
    
    -- | Check if coords are available.
    availCoords :: (Int, Int) -> Bool
    availCoords (x, y)
      | x >= 0 && x < width && y <= 0 &&  y > (-height) = True
      | otherwise = False


-- | Try place a mark a given position on a board.
-- The type of mark is determined automatically.
-- When the game is over (a winner exists) no marks are placed. 
putMarkAt :: (Int, Int) -> Board -> Board
putMarkAt (dx, dy) board = firstPart ++ [middle] ++ secondPart
  where
    firstPart = take (-dy) board
    middle = (updateAt dx board (fromMaybe [] (lookup dy zipBoard)))
    secondPart = drop ((-dy) + 1) board
    zipBoard = zip [0,-1..] board
 
-- | Choose mark of the next player.
changeMark :: Board -> Cell
changeMark board = if os < xs then Just O else Just X
     where
        os = length (filter (== (Just O)) ps)
        xs = length (filter (== (Just X)) ps)
        ps = concat board

-- | Try update an element at a given position in a list.
updateAt :: Int -> Board -> [Cell] -> [Cell]
updateAt dx board cells = insertAt (changeMark board) dx cells

-- | Insert at given position in a list.
insertAt ::(Num t, Eq t, Eq a) => Maybe a -> t -> [Maybe a] -> [Maybe a]
insertAt _newElement _i [] = []                     
insertAt newElement 0 (a:as) 
  | a == Nothing = newElement : as
  | otherwise = a : as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as


-- | Determine a winner in a game of tic-tac-toe (if exists).
winner :: Board -> Maybe (Mark, [(Int, Int)])
winner board = getWinnerLine (filter isLongStreak (concatMap streaks allLines))
  where
    allLines = rows ++ columns ++ diagonals
    
    rows = boardWithCoords board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)
    
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . transpose

-- | Create the 2d list with tuple of cell and coords
boardWithCoords :: Board -> [[(Maybe Mark, (Int,Int))]]
boardWithCoords board = take (length bwc - 1) bwc
  where
   bwc = listTo width (cellWithCoord (0, oneLineBoard) width)
   oneLineBoard = concat board
   width = length (head board)
  
-- | Make 1d list to the 2d list
listTo ::  Int -> [a]-> [[a]]
listTo width xs = chunks xs
  where
    chunks [] = []
    chunks list = take width list : chunks (drop width list)

-- | Create tuple of the cell and coords
cellWithCoord :: (Int, [a]) -> Int  ->  [(a, (Int,Int))]
cellWithCoord (_i, []) _width       = []
cellWithCoord (i, x:xs) width = (x, (dx, dy)) : (cellWithCoord ((i + 1), xs) width)
  where
    dx = i `mod`(fromIntegral width)
    dy = (-i `div` width)
    
-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: Eq a => [(Maybe a,(b,b))] -> [(a,[(b,b)])]
streaks [] = []
streaks ((Nothing, _coords) : xs) = streaks xs
streaks ((Just x,(dx, dy)) : xs) = (x,(dx,dy):(map snd ys)) : streaks zs
  where
    (ys, zs) = span (\(mark, _coords) -> mark == Just x) xs
    
-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: (Mark, [(Int,Int)]) -> Bool
isLongStreak (_mark, list) = length list >= 3

-- | Get a winning mark (if exists).
getWinnerLine :: [a] -> Maybe a
getWinnerLine xs = listToMaybe xs

run :: IO()
run = ticTacToe(initBoard (25,40))