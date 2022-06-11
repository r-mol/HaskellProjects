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
run = drawingOf (drawBoard (initBoard (5,4)))