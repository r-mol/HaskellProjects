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
sampleBoard = [ [ x, o, n, o, n ],
                [ n, o, n, x, o ],
                [ x, n, x, n, n ],
                [ n, o, n, x, x ] ] 
                where
                  (o, x, n) = (Just O, Just X, Nothing) 

-- | Initialise an empty NxM board.
initBoard :: (Int, Int) -> Board
initBoard (n, m) = replicate m (replicate n Nothing)

-- | Draw a single mark (X or O).
drawMark :: Mark -> Picture
drawMark X = scaled 0.4 0.4 (rotated (pi/4) (solidRectangle 0.5 2 <> solidRectangle 2 0.5))
drawMark O = thickCircle 0.2 0.3

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
drawBoard board = picturesOfX <> picturesOfO <> picturesOfN
  where 
    picturesOfX = pictures (picturesOf (Just X))
    picturesOfO = pictures (picturesOf (Just O))
    picturesOfN = pictures (picturesOf Nothing)
    picturesOf cell = map (drawCellAt cell) (positionsOf cell)
    positionsOf cell = concat(map (\(xs,y) -> zip (xs) (replicateOfYs xs y)) (coordinatesBoard cell))
    coordinatesBoard cell = (zip (map (positions cell) board) [0,-1..])
    replicateOfYs xs y = (replicate (length xs) y)
 

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
    
changeMark :: Board -> Cell
changeMark board = if os < xs then Just O else Just X
     where
        os = length (filter (== (Just O)) ps)
        xs = length (filter (== (Just X)) ps)
        ps = concat board



-- | Try update an element at a given position in a list.
updateAt :: Int -> Board -> [Cell] -> [Cell]
updateAt dx board cells = insertAt (changeMark board) dx cells

insertAt ::(Num t, Eq t, Eq a) => Maybe a -> t -> [Maybe a] -> [Maybe a]
insertAt _newElement _i [] = []                     
insertAt newElement 0 (a:as) 
  | a == Nothing = newElement : as
  | otherwise = a : as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as


-- | Determine a winner in a game of tic-tac-toe (if exists).
winner :: Board -> Maybe Mark
winner board = getWinner (filter isLongStreak (concatMap streaks allLines))
  where
    allLines = rows ++ columns ++ diagonals
    
    rows = board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)
    
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . transpose
    
-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: Eq a => [Maybe a] -> [(Int, a)]
streaks [] = []
streaks (Nothing : xs) = streaks xs
streaks (Just x : xs) = (1 + length ys, x) : streaks zs
  where
    (ys, zs) = span (== Just x) xs
    
-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: (Int, a) -> Bool
isLongStreak (i, _) = i >= 3

-- | Get a winning mark (if exists).
getWinner :: [(Int, a)] -> Maybe a
getWinner = listToMaybe . map snd

run :: IO()
run = ticTacToe (initBoard (5,4))
  -- drawingOf (drawBoard(putMarkAt (4,-3) (initBoard (5,4))))