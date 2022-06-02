module Project where

import CodeWorld

-- | A fractal tree of a given rank.
tree :: Double -> Picture
tree 0 = blank
tree n = solidPolygon [(-n/70, n/5), (n/70, n/5) ,(n/45,0),(-n/45, 0)] 

treeN :: Double -> Picture
treeN 0 = blank
treeN n = segment <> translated 0 (n/7) (leftBranch <> rightBranch) <> translated 0 (n/6) leaf
  where
    segment = solidPolygon [(-n/70, n/5), (n/70, n/5) ,(n/45,0),(-n/45, 0)]
    
    leftBranch  = rotated (pi/8) (treeN (n - 1))
    rightBranch = rotated (-pi/8) (treeN (n - 1))
    
    leaf
        | n == 1 = colored green (solidCircle 0.1)
        | otherwise = blank

treeT :: Double -> Picture
treeT n
    | n > 0 && n < 8 = tree n <> translated 0 (n/7 ) (colored green(solidCircle 0.07) <> rotated (pi/8) (treeT (n - 1)) <> rotated (-pi/8) (treeT (n - 1) )<>colored green(solidCircle 0.07))
    | n <= 0 = blank
    | otherwise = treeN 8
    

run :: IO ()
run = animationOf (treeT)

