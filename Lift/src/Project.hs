{-# LANGUAGE OverloadedStrings #-}
module Project where

import CodeWorld
-- | Elevator button.
data Button = Up | Down | Stop

-- | Colors.
data Colors = Red | Grey | White | Pink

-- | Convert from data Colors to standart Color
colors :: Colors -> Color
colors Red   = red
colors Grey  = grey
colors White = white
colors Pink  = pink


arrow :: Colors -> Picture
arrow color = colored (colors color) (solidPolygon([(0, -0.4), (0.7, -0.7), (0, 1), (-0.7, -0.7)]))


rotatedArrow :: Point -> Double -> Colors-> Picture
rotatedArrow (x, y) delta color = translated x y (rotated delta( arrow color))


frameOfController :: Picture
frameOfController = colored (light grey) (solidRectangle 1.9 4.4) <> colored black (solidRectangle 2 4.5)


elevatorController :: Picture
elevatorController = rotatedArrow (0, 1) 0 Grey <> rotatedArrow (0, -1) pi Grey <> frameOfController


frameOfButton :: Picture
frameOfButton = colored grey (solidCircle 1.2) <> colored black (solidCircle 1.4)


buttonOfMoving :: Double -> Picture
buttonOfMoving delta = rotatedArrow (0, 0) delta White <> frameOfButton


buttonOfStoping :: Picture
buttonOfStoping = colored red (lettering "Stop") <> frameOfButton


-- | Render elevator button.
drawButton :: Button ->Picture
drawButton Up   = buttonOfMoving 0
drawButton Down = buttonOfMoving pi
drawButton Stop = buttonOfStoping

-- | Draw several objects
-- some distance apart from each other. 
asSpaced
  :: Double -- ˆ How far apart to draw objects. 
  -> (a -> Picture) -- ˆ How to draw a single object. 
  -> [a] -- ˆ A list of objects to draw.
  -> Picture
asSpaced distance drawFunction objects
  | (length objects) == 0 = blank
  | otherwise     = translated distance 0 (drawFunction (head objects)) <> (asSpaced (distance + 5) drawFunction (tail objects))
  
run :: IO ()
run = solution1


solution1 :: IO ()
solution1 = drawingOf (elevatorController <> asSpaced 5 drawButton [Up,Down,Stop] )