{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Project where

import CodeWorld

-- | Elevator button.
data Button = G_Up | G_Down | G_Stop

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
drawButton G_Up   = buttonOfMoving 0
drawButton G_Down = buttonOfMoving pi
drawButton G_Stop = buttonOfStoping

-- | Draw several objects
-- some distance apart from each other. 
asSpaced
  :: Double         -- ˆ How far apart to draw objects. 
  -> (a -> Picture) -- ˆ How to draw a single object. 
  -> [a]            -- ˆ A list of objects to draw.
  -> Picture
asSpaced distance drawFunction objects
  | (length objects) == 0 = blank
  | otherwise     = translated distance 0 (drawFunction (head objects)) <> (asSpaced (distance + 3) drawFunction (tail objects))
  
-- | Elevator mode of operation.
data Mode = M_Up | M_Down | Idle

-- | Some definition of action for FSM.
type Action = Button


-- | Transition by an action to a new state.
type Transition = (Action, Mode)

-- | Render elevator mode.
drawMode :: Mode -> Picture
drawMode M_Up   = rotatedArrow (0, 1) 0 Red <> elevatorController <> asSpaced 3 drawButton [G_Stop]
drawMode M_Down = rotatedArrow (0, -1) 0 Red <> elevatorController <> asSpaced 3 drawButton [G_Stop]
drawMode Idle   = elevatorController <> asSpaced 3 drawButton [G_Up,G_Down]

-- | Initial state of an FSM.
initialState :: Mode
initialState = Idle
    
-- | All transitions for a given state.
elevator :: Mode -> [Transition]
elevator Idle   = [(G_Up, M_Up), (G_Down, M_Down)]
elevator M_Up   = [(G_Stop, Idle)]
elevator M_Down = [(G_Stop, Idle)]

-- | Predicate checking if a given state is final.
isSame :: Action -> Action -> Bool
isSame G_Up G_Up = True
isSame G_Down G_Down = True
isSame G_Stop G_Stop = True
isSame _ _ = False

-- | Apply an action (if any) to the current state
-- of a finite state machine.
applyAction
  :: Maybe a          -- ˆ An action to apply (if any).
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)])  -- ˆ State transitions.
  -> s                -- ˆ Current state.
  -> s                -- ˆ New state (if possible).
applyAction (Just action) isS transitionOf curMode
  | length transitions == 0 = curMode
  | otherwise = snd (head recursiveLangs)
    where
      transitions = (transitionOf curMode)
      recursiveLangs = filter (\e -> transitionLang e) transitions
      transitionLang (ac, _sc) = isS action ac
        
applyAction Nothing _isS _transitionOf curMode = curMode

run :: IO ()
run = do
  activityOf initialWorld handleWorld renderWorld
    where    
      -- | Start of the actor.
      initialWorld :: Mode
      initialWorld = Idle
      
      -- | Changing the world and position of the actor and the world
      handleWorld :: Event -> Mode -> Mode
      handleWorld (TimePassing dt) mode   = (updateWorld dt mode)
      handleWorld (PointerPress (3,0)) Idle = applyAction (Just G_Up) isSame elevator Idle
      handleWorld (PointerPress (6,0)) Idle = applyAction (Just G_Down) isSame elevator Idle
      handleWorld (PointerPress (3,0)) M_Up  = applyAction (Just G_Stop) isSame elevator M_Up
      handleWorld (PointerPress (3,0)) M_Down  = applyAction (Just G_Stop) isSame elevator M_Down
      handleWorld _anyEvent mode          = mode
     
      -- | Render actor and world with new data.
      renderWorld :: Mode -> Picture
      renderWorld mode = drawMode mode
    
    
      -- | Update world by time.
      updateWorld :: Double -> Mode -> Mode
      updateWorld _dt = id


solution1 :: IO ()
solution1 = drawingOf (elevatorController <> asSpaced 5 drawButton [G_Up,G_Down,G_Stop] )

solution2 :: IO ()
solution2 = do
  activityOf initialWorld handleWorld renderWorld
    where    
      -- | Start of the actor.
      initialWorld :: Mode
      initialWorld = Idle
      
      -- | Changing the world and position of the actor and the world
      handleWorld :: Event -> Mode -> Mode
      handleWorld (TimePassing dt) mode   = (updateWorld dt mode)
      handleWorld (KeyPress "Up") mode    = applyAction (Just G_Up) isSame elevator mode
      handleWorld (KeyPress "Down") mode  = applyAction (Just G_Down) isSame elevator mode
      handleWorld (KeyPress " ") mode     = applyAction (Just G_Stop) isSame elevator mode
      handleWorld _anyEvent mode          = mode
     
      -- | Render actor and world with new data.
      renderWorld :: Mode -> Picture
      renderWorld mode = drawMode mode
    
    
      -- | Update world by time.
      updateWorld :: Double -> Mode -> Mode
      updateWorld _dt = id