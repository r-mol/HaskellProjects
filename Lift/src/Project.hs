{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Project where

import CodeWorld

-- | Colors.
data Colors = Red | Grey | White | Pink

-- | Convert from data Colors to standart Color
colors :: Colors -> Color
colors Red   = red
colors Grey  = grey
colors White = white
colors Pink  = pink


sign :: Colors -> Picture
sign color = colored (colors color) (solidPolygon([(0, -0.4), (0.7, -0.7), (0, 1), (-0.7, -0.7)]))


rotatedSign :: Point -> Double -> Colors-> Picture
rotatedSign (x, y) delta color = translated x y (rotated delta( sign color))


frameOfController :: Picture
frameOfController = colored (light grey) (solidRectangle 1.9 4.4) <> colored black (solidRectangle 2 4.5)


elevatorController :: Picture
elevatorController = rotatedSign (0, 1) 0 Grey <> rotatedSign (0, -1) pi Grey <> frameOfController


frameOfButton :: Picture
frameOfButton = colored grey (solidCircle 1.2) <> colored black (solidCircle 1.4)


buttonOfMoving :: Double -> Picture
buttonOfMoving delta = rotatedSign (0, 0) delta White <> frameOfButton


buttonOfStoping :: Picture
buttonOfStoping = colored red (lettering "Stop") <> frameOfButton

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
data State = UpMove | DownMove | Idle

-- | Some definition of action for FSM.
data Action = UpButton | DownButton | StopButton


-- | Transition by an action to a new state.
type Transition = (Action, State)

-- | Apply an action (if any) to the current state
-- of a finite state machine.
applyAction
  :: Maybe a          -- ˆ An action to apply (if any).
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)])  -- ˆ State transitions.
  -> s               -- ˆ Current state.
  -> s              -- ˆ New state (if possible).
  
applyAction Nothing _isS _transitionOf state = state
applyAction (Just action) isSame transitionOf state
  | length transitions == 0 = state
  | length recursiveLangs == 0 = state
  | otherwise = snd (head recursiveLangs)
    where
      transitions = (transitionOf state)
      recursiveLangs = filter (\e -> transitionLang e) transitions
      transitionLang (ac, _sc) = isSame action ac
      
-- | Interactive finite state machine simulation.
interactiveFSM
  :: s                  -- ˆ Initial state.
  -> (a -> a -> Bool)   -- ˆ Action equality test.
  -> (s -> [(a, s)])    -- ˆ State transitions.
  -> (Event -> Maybe a) -- ˆ How to convert events into actions.
  -> (s -> Picture)     -- ˆ How to draw states.
  -> (a -> Picture)     -- ˆ How to draw actions.
  -> IO ()
interactiveFSM initState isSame elevator converterOf drawState _drawAction = 
  activityOf initState handleWorld renderWorld
    where
      -- | Changing the world and position of the actor and the world
      handleWorld :: Event -> s -> s
      handleWorld (KeyPress key) state = (applyAction (converterOf (KeyPress key)) isSame elevator state)
      handleWorld _anyEvent state = state
     
      -- | Render actor and world with new data.
      renderWorld :: s -> Picture
      renderWorld state = (drawState state)
      

run :: IO ()
run = solution3

solution1 :: IO ()
solution1 = do
  drawingOf (elevatorController <> asSpaced 5 drawAction [UpButton,DownButton,StopButton])
      where
        -- | Render elevator button.
        drawAction :: Action -> Picture
        drawAction UpButton   = buttonOfMoving 0
        drawAction DownButton = buttonOfMoving pi
        drawAction StopButton = buttonOfStoping

solution2 :: IO ()
solution2 = do
  activityOf initialWorld handleWorld renderWorld
    where    
      -- | Start of the actor.
      initialWorld :: State
      initialWorld = Idle
      
      -- | Changing the world and position of the actor and the world
      handleWorld :: Event -> State -> State
      handleWorld (KeyPress key) state    
        | key == "Up"   = applyAction (Just UpButton) isSame elevator state
        | key == "Down" = applyAction (Just DownButton) isSame elevator state
        | key == " "    = applyAction (Just StopButton) isSame elevator state
        | otherwise     = state
      handleWorld _anyEvent state = state
     
      -- | Render actor and world with new data.
      renderWorld :: State -> Picture
      renderWorld state = drawState state
    
      -- | Predicate checking if a given state is final.
      isSame :: Action -> Action -> Bool
      isSame UpButton UpButton     = True
      isSame DownButton DownButton = True
      isSame StopButton StopButton = True
      isSame _ _ = False
      
      -- | All transitions for a given state.
      elevator :: State -> [Transition]
      elevator Idle   = [(UpButton, UpMove), (DownButton, DownMove)]
      elevator UpMove   = [(StopButton, Idle)]
      elevator DownMove = [(StopButton, Idle)]
      
      -- | Render elevator mode.
      drawState :: State -> Picture
      drawState UpMove   = rotatedSign (0, 1) 0 Red <> elevatorController <> asSpaced 3 drawAction [StopButton]
      drawState DownMove = rotatedSign (0, -1) pi Red <> elevatorController <> asSpaced 3 drawAction [StopButton]
      drawState Idle   = elevatorController <> asSpaced 3 drawAction [UpButton,DownButton]
      
      -- | Render elevator button.
      drawAction :: Action ->Picture
      drawAction UpButton   = buttonOfMoving 0
      drawAction DownButton = buttonOfMoving pi
      drawAction StopButton = buttonOfStoping


solution3 :: IO ()
solution3 = do
  interactiveFSM initState isSame elevator converter drawState drawAction
    where    
       -- | Initial state of an FSM.
      initState :: State
      initState = Idle
      
      -- | Predicate checking if a given state is final.
      isSame :: Action -> Action -> Bool
      isSame UpButton UpButton     = True
      isSame DownButton DownButton = True
      isSame StopButton StopButton = True
      isSame _ _ = False
      
      -- | All transitions for a given state.
      elevator :: State -> [Transition]
      elevator Idle   = [(UpButton, UpMove), (DownButton, DownMove)]
      elevator UpMove   = [(StopButton, Idle)]
      elevator DownMove = [(StopButton, Idle)]
      
      -- | Changing the world and position of the actor and the world
      converter :: Event -> Maybe Action
      converter (KeyPress "Up")    = Just UpButton
      converter (KeyPress "Down")  = Just DownButton
      converter (KeyPress " ")     = Just StopButton
      converter _anyEvent          = Nothing
     
    
      -- | Render elevator mode.
      drawState :: State -> Picture
      drawState UpMove   = rotatedSign (0, 1) 0 Red <> elevatorController <> asSpaced 3 drawAction [StopButton]
      drawState DownMove = rotatedSign (0, -1) pi Red <> elevatorController <> asSpaced 3 drawAction [StopButton]
      drawState Idle   = elevatorController <> asSpaced 3 drawAction [UpButton,DownButton]

      -- | Render elevator button.
      drawAction :: Action -> Picture
      drawAction UpButton   = buttonOfMoving 0
      drawAction DownButton = buttonOfMoving pi
      drawAction StopButton = buttonOfStoping