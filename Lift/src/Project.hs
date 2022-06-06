{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Project where

import CodeWorld

-- | Colors.
data Colors = Red | Grey | White 

-- | Elevator mode of operation.
data State = UpMove | DownMove | Idle

-- | Some definition of action for FSM.
data Action = UpButton | DownButton | StopButton

-- | Transition by an action to a new state.
type Transition = (Action, State)

-- | System of the elevator.
type System = Double

-- | Convert from data Colors to standart Color.
colors :: Colors -> Color
colors Red   = red
colors Grey  = grey
colors White = white

-- | The polygon for the sign of the elevator.
sign :: Colors -> Picture
sign color = colored (colors color) (solidPolygon([(0, -0.4), (0.7, -0.7), (0, 1), (-0.7, -0.7)]))

-- | Rotated sign.
rotatedSign :: Point -> Double -> Colors-> Picture
rotatedSign (x, y) delta color = translated x y (rotated delta( sign color))

-- | Frame of elevator controller.
frameOfController :: Picture
frameOfController = colored (light grey) (solidRectangle 1.9 4.4) <> colored black (solidRectangle 2 4.5)

-- | Elevator controller.
elevatorController :: Picture
elevatorController = rotatedSign (0, 1) 0 Grey <> rotatedSign (0, -1) pi Grey <> frameOfController

-- | Elevator controller for "Up mode".
upController :: Picture
upController = rotatedSign (0, 1) 0 Red <> elevatorController

-- | Elevator controller for "Down mode".
downController :: Picture
downController = rotatedSign (0, -1) pi Red <> elevatorController

-- | Frame of Button.
frameOfButton :: Picture
frameOfButton = colored grey (solidCircle 1.2) <> colored black (solidCircle 1.4)

-- | Button with the rotated sign.
buttonOfMoving :: Double -> Picture
buttonOfMoving delta = rotatedSign (0, 0) delta White <> frameOfButton

-- | Button with text "STOP".
buttonOfStoping :: Picture
buttonOfStoping = colored red (lettering "Stop") <> frameOfButton

-- | Elevator.
elevatorPicture :: System ->  Picture
elevatorPicture height = translated 0 height (lettering "\x1F6B6" <> colored white (solidRectangle 2 4) <> colored black (solidRectangle 2.5 4.5))

-- | Draw the elevator.
drawSystem :: System -> Picture
drawSystem height = elevatorPicture height

-- | Comparisson of the action.
instance Eq Action where
  UpButton == UpButton = True
  DownButton == DownButton = True
  StopButton == StopButton = True
  _ == _ = False

-- | Draw several objects
-- some distance apart from each other. 
asSpaced
  :: Double         -- ˆ How far apart to draw objects. 
  -> (a -> Picture) -- ˆ How to draw a single object. 
  -> [a]            -- ˆ A list of objects to draw.
  -> Picture
asSpaced _ _ [] = blank
asSpaced dis f (n:ns) = f n <> translated dis 0 (asSpaced dis f ns)
  
-- | Apply an action (if any) to the current state
-- of a finite state machine.
applyAction
  :: Maybe a          -- ˆ An action to apply (if any).
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)])  -- ˆ State transitions.
  -> s                -- ˆ Current state.
  -> s                -- ˆ New state (if possible).
applyAction Nothing _isS _transitionOf state = state
applyAction (Just action) isS transitionOf state
  | length transitions == 0 = state
  | length recursiveLangs == 0 = state
  | otherwise = snd (head recursiveLangs)
    where
      transitions = (transitionOf state)
      recursiveLangs = filter (\e -> transitionLang e) transitions
      transitionLang (ac, _sc) = isS action ac
      
-- | Interactive finite state machine simulation.
interactiveFSM
  :: s                  -- ˆ Initial state.
  -> (a -> a -> Bool)   -- ˆ Action equality test.
  -> (s -> [(a, s)])    -- ˆ State transitions.
  -> (Event -> Maybe a) -- ˆ How to convert events into actions.
  -> (s -> Picture)     -- ˆ How to draw states.
  -> (a -> Picture)     -- ˆ How to draw actions.
  -> IO ()
interactiveFSM initState isSame elevator converterOf drawState drawAction = 
  activityOf initState handleWorld renderWorld
    where
      -- | Changing the position of the elevator in a world.
      handleWorld (KeyPress key) state = (applyAction (converterOf (KeyPress key)) isSame elevator state)
      handleWorld _anyEvent state = state

      -- | Combine all drawing pictures in a one list.
      listOfPictures state = drawState state : map (drawAction . fst) (elevator state)

      -- | Render states with new data.
      renderWorld state = asSpaced 3 id (listOfPictures state)
      
interactiveSystem
  :: s                   -- ˆ Initial state of a FSM.
  -> (a -> a -> Bool)    -- ˆ FSM action equality test.
  -> (s -> [(a, s)])     -- ˆ FSM State transitions.
  -> (Event -> Maybe a)  -- ˆ How to convert events into actions.
  -> (s -> Picture)      -- ˆ How to draw states.
  -> (a -> Picture)      -- ˆ How to draw actions.
  
  -> system              -- ˆ System state, whose modes
                         -- are modelled with FSM.

  -> (Double -> s -> system -> system)
                         -- ˆ How system evolves with time.
  -> (system -> Picture) -- ˆ How to render system.
  -> IO ()
interactiveSystem initState isSame elevator converterOf drawState drawAction system changeByTime drawrSystem =
  activityOf (initState, system) handleSystem renderSystem
    where
      -- | Changing the position of the elevator in a world.
      handleSystem (TimePassing dt) (state, newSystem) = ((applyAction (converterOf (TimePassing dt)) isSame elevator state), (changeByTime  dt state newSystem))
      handleSystem event (state, newSystem) = (applyAction (converterOf event) isSame elevator state, newSystem)
        
      -- | Combine all drawing pictures in a one list.
      listOfPictures (state, newSystem) = drawrSystem newSystem : drawState state: map (drawAction . fst) (elevator state)
        
      -- | Render elevator with new data.
      renderSystem (state, newSystem) = asSpaced 3 id (listOfPictures (state, newSystem))

run :: IO ()
run = solution4

solution1 :: IO ()
solution1 = drawingOf (asSpaced 3 id (listOfPictures Idle))
  where
    -- | Render elevator mode.
    drawState :: State -> Picture
    drawState UpMove   = upController
    drawState DownMove = downController
    drawState Idle     = elevatorController

    -- | Render elevator button.
    drawAction :: Action -> Picture
    drawAction UpButton   = buttonOfMoving 0
    drawAction DownButton = buttonOfMoving pi
    drawAction StopButton = buttonOfStoping

    -- | Combine all drawing pictures in a one list.
    listOfPictures state = drawState state : map (drawAction . fst) (elevator state)

    -- | All transitions for a given state.
    elevator :: State -> [Transition]
    elevator Idle     = [(UpButton, UpMove), (DownButton, DownMove)]
    elevator UpMove   = [(StopButton, Idle)]
    elevator DownMove = [(StopButton, Idle)]

solution2 :: IO ()
solution2 = activityOf initState handleWorld renderWorld
  where
     -- | Initial state of an FSM.
    initState :: State
    initState = Idle

    -- | Changing the position of the elevator in a world.
    handleWorld :: Event -> State -> State
    handleWorld (KeyPress key) state    
      | key == "Up"   = applyAction (Just UpButton) isSame elevator state
      | key == "Down" = applyAction (Just DownButton) isSame elevator state
      | key == " "    = applyAction (Just StopButton) isSame elevator state
      | otherwise     = state
    handleWorld _anyEvent state = state

    -- | Combine all drawing pictures in a one list.
    listOfPictures state = drawState state : map (drawAction . fst) (elevator state)

    -- | Render actor and world with new data.
    renderWorld :: State -> Picture
    renderWorld state = asSpaced 3 id (listOfPictures state)
    
    -- | Predicate checking if a given state is final.
    isSame :: Eq a => a -> a -> Bool
    isSame x y = x == y

    -- | All transitions for a given state.
    elevator :: State -> [Transition]
    elevator Idle   = [(UpButton, UpMove), (DownButton, DownMove)]
    elevator UpMove   = [(StopButton, Idle)]
    elevator DownMove = [(StopButton, Idle)]
    
    -- | Render elevator mode.
    drawState :: State -> Picture
    drawState UpMove   = upController
    drawState DownMove = downController
    drawState Idle   = elevatorController

    -- | Render elevator button.
    drawAction :: Action -> Picture
    drawAction UpButton   = buttonOfMoving 0
    drawAction DownButton = buttonOfMoving pi
    drawAction StopButton = buttonOfStoping

solution3 :: IO ()
solution3 = interactiveFSM initState isSame elevator converter drawState drawAction
  where
   -- | Initial state of an FSM.
    initState :: State
    initState = Idle

    -- | Predicate checking if a given state is final.
    isSame :: Eq a => a -> a -> Bool
    isSame x y = x == y

    -- | All transitions for a given state.
    elevator :: State -> [Transition]
    elevator Idle   = [(UpButton, UpMove), (DownButton, DownMove)]
    elevator UpMove   = [(StopButton, Idle)]
    elevator DownMove = [(StopButton, Idle)]

    -- | Changing the position of the elevator in a world.
    converter :: Event -> Maybe Action
    converter (KeyPress "Up")    = Just UpButton
    converter (KeyPress "Down")  = Just DownButton
    converter (KeyPress " ")     = Just StopButton
    converter _anyEvent          = Nothing

    -- | Render elevator mode.
    drawState :: State -> Picture
    drawState UpMove   = upController
    drawState DownMove = downController
    drawState Idle   = elevatorController

    -- | Render elevator button.
    drawAction :: Action -> Picture
    drawAction UpButton   = buttonOfMoving 0
    drawAction DownButton = buttonOfMoving pi
    drawAction StopButton = buttonOfStoping

solution4 :: IO ()
solution4 = interactiveSystem initState isSame elevator converter drawState drawAction 0 changeByTime drawSystem 
  where
     -- | Initial state of an FSM.
    initState :: State
    initState = Idle

    -- | Predicate checking if a given state is final.
    isSame :: Eq a => a -> a -> Bool
    isSame x y = x == y

    -- | All transitions for a given state.
    elevator :: State -> [Transition]
    elevator Idle   = [(UpButton, UpMove), (DownButton, DownMove)]
    elevator UpMove   = [(StopButton, Idle)]
    elevator DownMove = [(StopButton, Idle)]

    -- | Changing the position of the elevator in a world.
    converter :: Event -> Maybe Action
    converter (KeyPress "Up")    = Just UpButton
    converter (KeyPress "Down")  = Just DownButton
    converter (KeyPress " ")     = Just StopButton
    converter _anyEvent          = Nothing

    -- | Change system by time passing.
    changeByTime :: Double -> State -> System -> System
    changeByTime dt UpMove heightOld = heightOld + dt
    changeByTime dt DownMove heightOld = heightOld - dt
    changeByTime _ _ heightOld = heightOld

    -- | Render elevator mode.
    drawState :: State -> Picture
    drawState UpMove   = upController
    drawState DownMove = downController
    drawState Idle   = elevatorController

    -- | Render elevator button.
    drawAction :: Action -> Picture
    drawAction UpButton   = buttonOfMoving 0
    drawAction DownButton = buttonOfMoving pi
    drawAction StopButton = buttonOfStoping
  