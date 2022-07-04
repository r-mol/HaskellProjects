module Project where

import Graphics.Gloss
import Numeric.Noise.Perlin
import System.Random

-- width of each square
res :: Float
res = 15

win :: Display
win = InWindow "Marching Squares" scDims (0,0)

screenW :: Float
screenW = 1000
screenH :: Float
screenH = 700

scDims :: (Int, Int)
scDims = (floor screenW, floor screenH)

-- number of rows/cols in the grid
w :: Float
w = screenW / res
h :: Float
h = screenH / res

-- Radius of each circle in the grid
circR :: Float
circR = res / 4

-- middle of the window
midW :: Float
midW = screenW / 2 + 0.5 * res
midH :: Float
midH = screenH / 2 + 0.5 * res

points :: [(Float, Float)]
points = [(x,y) | x <- [1..w], y <- [1..h]]

run :: IO ()
run = do
        seed <- randomRIO (0,1000)          -- randomly-generated seed for the Noise generation
        let ns = perlin seed 5 0.05 0.5     -- generate Perlin Noise object for the underlying grid function
        --putStrLn ("9 -> " ++ show (case2Corners 9))
        animate win black (getPicture ns)


getPicture :: Perlin -> Float -> Picture
getPicture ns t = Pictures $ (point2Pic <$> points) <*> [ns] <*> [t]



-- Points in the grid (2D array of width w and height h)
pointss :: [(Float, Float)]
pointss = [(x,y) | x <- [1..w], y <- [1..h]]

point2Pic :: (Float, Float) -> Perlin -> Float -> Picture
point2Pic (x,y) ns t = translate (res * x - midW) (res * y - midH) pics
    where
        corners  = getCorners (x,y) ns t
        cellcase = corners2Case corners
        dotCol :: Color
        dotCol = val2Color (head corners)
        lineCol = points2Color corners
        dot = color dotCol $ circleSolid circR
        lines = caseToLines cellcase corners (x,y) 
        linesColoured = if y < h && x < w then color lineCol . scale res res <$> lines
                        else []
        pics = Pictures (dot:linesColoured)


-- Get the noise values at each corner to the given point
getCorners :: (Float, Float) -> Perlin -> Float -> Corners
getCorners (x,y) ns t = [
                          noiseAt (x,   y  ) ns t,     -- north west
                          noiseAt (x+1, y  ) ns t,     -- north east
                          noiseAt (x+1, y+1) ns t,     -- south east
                          noiseAt (x  , y+1) ns t      -- south west
                        ]


type Corners = [Float]


-- Map the binary corner states to an integer
-- e.g. (0,0,1,0) -> 2
corners2Case :: Corners -> Int
corners2Case corners = nw * 8 + ne * 4 + se * 2 + sw -- * 1
    where 
        [nw,ne,se,sw] = map round corners
        -- mx = maximum corners
        -- mn = minimum corners
        -- mid = mn + (mx - mn) / 2
        -- partition :: Float -> Int
        -- partition f = if f > mid then 1 else 0

case2Corners :: Int -> Corners
case2Corners x | x < 0 || x > 15 = [0,0,0,0]
case2Corners 0 = []
case2Corners 15 = [1,1,1,1]
case2Corners x = divX x 8

-- Repeatedly divides x, halving the divisor at each iteration
-- Returns a binary array of the quotients
divX :: Int -> Int -> [Float]
divX x 0 = []
divX x 1 = [realToFrac x]
divX x n = realToFrac q : divX (x - q * n) (n `div` 2)     -- get the remaining bits, recursively
    where 
        q = x `div` n   -- quotient, i.e. the next bit in the array
        r = x - q*n     -- remainder after the division

caseToLines :: Int -> Corners -> (Float,Float) -> [Picture]
caseToLines c [nw,ne,se,sw] (x,y) = case c of
                                        1  -> [Line [westSplit ,  southSplit ]]
                                        2  -> [Line [southSplit,  eastSplit  ]]
                                        3  -> [Line [westSplit ,  eastSplit  ]]
                                        4  -> [Line [northSplit,  eastSplit  ]]
                                        5  -> [Line [westSplit ,  northSplit ], Line [southSplit , eastSplit ]]
                                        6  -> [Line [southSplit,  northSplit ]]
                                        7  -> [Line [westSplit ,  northSplit ]]
                                        8  -> [Line [northSplit ,  westSplit ]]
                                        9  -> [Line [northSplit,  southSplit ]]
                                        10 -> [Line [westSplit ,  southSplit ], Line [northSplit , eastSplit ]]
                                        11 -> [Line [northSplit,  eastSplit  ]]
                                        12 -> [Line [westSplit ,  eastSplit  ]]
                                        13 -> [Line [southSplit,  eastSplit  ]]
                                        14 -> [Line [westSplit ,  southSplit ]]
                                        _  -> []--scale (1/res) (1/res) <$> [translate (res * mx) (res * my) $ Color red $ Circle circR]
    where
        westSplit = splitVt nw sw (0,0) (0,1)
        northSplit = splitHz nw ne (0,0) (1,0)
        eastSplit = splitVt ne se (1,0) (1,1)
        southSplit = splitHz sw se (0,1) (1,1)
        (mx, my) = southSplit

caseToLines _ _ _ = []


splitVt :: Float -> Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
splitVt a b (ax,ay) (bx,by) = (ax, ay + r * (by - ay))
    where r = linInter a b 0.5

splitHz :: Float -> Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
splitHz a b (ax,ay) (bx,by) = (ax + r * (bx - ax), ay)
    where r = linInter a b 0.5

linInter :: Float -> Float -> Float -> Float
linInter a b m = (m - a) / (b - a)


point2Color :: (Float,Float) -> Perlin -> Float -> Color
point2Color pt ns t = val2Color $ noiseAt pt ns t

val2Color :: Float -> Color
val2Color val | val > 0.5 && val <= 1  = greyN $ val * val
              | otherwise              = greyN 0


-- Gets the noise at the given point (x,y) at time t in the animation
-- Returns a normalised value between 0 and 1
noiseAt :: (Float, Float) -> Perlin -> Float -> Float
noiseAt (x,y) ns t = (val + 1) / 2
    where
        (dx,dy,dt) = (realToFrac x, realToFrac y, realToFrac t)
        val = realToFrac $ noiseValue ns (dx,dy,dt)

points2Color :: Corners -> Color 
points2Color [nw,ne,se,sw] = makeColor r g b 1
    where
        avg = (nw + ne + se + sw) / 4
        r = avg
        g = 0.4 + 0.6 * avg
        b = 0.7 + 0.3 * avg
points2Color _ = white 