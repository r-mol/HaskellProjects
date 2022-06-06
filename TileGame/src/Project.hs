{-# LANGUAGE OverloadedStrings #-}
module Project where


{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | A tile (poor man's representation):
--
-- * 0 — floor
-- * 1 — wall
-- * 2 — exit
-- * 3 — blue button
-- * 4 — blue door
-- * 5 — red button
-- * 6 — red door
-- * otherwise — (?)

data State = State Coords [Color]
data Colors = Red | Blue | Green | Pink
data Tile = Wall | Floor | Door Colors | Exit | Button Colors
data Dir = R | L | U | D
type Coords = (Double, Double)

-- | Interaction state for 'world' with start screen.
data WithStartScreen world
  = StartScreen -- ˆ Start screen.
  | GameOn world -- ˆ Game is on with 'world' state.

-- | The type of an 'activityOf' function.
type ActivityOf world 
 = world
 -> (Event -> world -> world)
 -> (world -> Picture)
 -> IO()


-- | List of colors with coords for Buttons.
buttons :: [(Colors, Coords)]
buttons = [(Green, (-1, 6)), (Green, (-2, 6)), (Green, (-3, 5)),
           (Green, (-4, 5)), (Green, (-4, 4)), (Green, (-5, 4)),
           (Green, (-5, 3)), (Green, (-6, 2)), (Green, (-6, 1)),
           (Green, (1, -6)), (Green, (2, -6)), (Green, (3, -5)),
           (Green, (4, -5)), (Green, (4, -4)), (Green, (5, -4)),
           (Green, (5, -3)), (Green, (6, -2)), (Green, (6, -1)),
           (Red, (3, 1)), (Red, (2, 2)), (Red, (1, 3)),
           (Red, (-1, -3)), (Red, (-3, -1)), (Red, (-2, -2)),
           (Blue, (-1, 9)),(Blue, (1, 9)) , (Pink, (-9, 1)),(Pink, (-9, -1)), 
           (Pink, (9, 1)),(Pink, (9, -1))]


-- | List of colors with coords for Doors.
doors :: [(Colors, Coords)]
doors = [(Green, (1, 5)), (Green, (2, 5)), (Green, (3, 4)),
         (Green, (4, 3)), (Green, (5, 2)), (Green, (5, 1)),
         (Green, (-1, -5)), (Green, (-2, -5)), (Green, (-3, -4)),
         (Green, (-4, -3)), (Green, (-5, -2)), (Green, (-5, -1)),
         (Green, (-9, 0)), (Red, (-2, 1)), (Red, (-1, 2)), (Red, (2, -1)),
         (Red, (1, -2)), (Red, (-4, 3)), (Red, (4, -3)),
         (Red, (9, 0)), (Blue, (0, 6)), (Blue, (0, -6)),
         (Pink, (-8, 1)), (Pink, (-8, 2)), (Pink, (-7, 3)),
         (Pink, (-7, 4)), (Pink, (-6, 5)), (Pink, (-6, 6)),
         (Pink, (-5, 6)), (Pink, (-4, 7)), (Pink, (-3, 7)),
         (Pink, (-2, 8)), (Pink, (-1, 8)),
         (Pink, (8, -1)), (Pink, (8, -2)), (Pink, (7, -3)),
         (Pink, (7, -4)), (Pink, (6, -5)), (Pink, (6, -6)),
         (Pink, (5, -6)), (Pink, (4, -7)), (Pink, (3, -7)),
         (Pink, (2, -8)), (Pink, (1, -8))]


-- | A picture of a floor tile or size 0.95x0.95.
floorTile :: Picture
floorTile = colored yellow (solidRectangle 0.95 0.95)


-- | A picture of a wall tile or size 0.95x0.95.
wallTile :: Picture
wallTile = colored black (solidRectangle 0.95 0.95)


-- | A picture of a button tile with colored circle or size 0.95x0.95 and radius 0.4.
buttonTile :: Color -> Picture
buttonTile c = coloredCircle <> yellowSquare
  where
     coloredCircle = colored c (solidCircle 0.4) 
     yellowSquare = colored yellow (solidRectangle 0.95 0.95)


-- | A picture of a door tile with colored circle or size 0.95x0.95 and radius 0.4.
doorTile :: Color -> Picture
doorTile c = coloredCircle <> blackSquare
  where
    coloredCircle = colored c (solidCircle 0.4) 
    blackSquare = colored black (solidRectangle 0.95 0.95)


-- | A picture of a exit tile or size 0.95x0.95.
exitTile :: Picture
exitTile = tinySquare <> smallSquare <> mediumSquare <> bigSquare <> blackSquare
  where
    tinySquare   = colored red (rectangle 0.1 0.1) 
    smallSquare  = colored red (rectangle 0.3 0.3) 
    mediumSquare = colored red (rectangle 0.5 0.5) 
    bigSquare    = colored red (rectangle 0.7 0.7) 
    blackSquare  = colored black (solidRectangle 0.95 0.95)


-- | Picture of start screen
startScreen :: Picture
startScreen = text <> rule
  where
    text = scaled 3 3 (lettering "Escape the Room")
    rule = translated 0 (-5) (lettering "[press Space to start]")
  
  
-- | Check coordinates to draw the circle.
circleByTiles :: Coords -> Integer -> Bool
circleByTiles (x,y) radius = 
  round (sqrt((abs x )^2 + (abs y)^2)) >= radius 
  && round (sqrt((abs x )^2 + (abs y)^2)) < radius + 1
  
-- | Check coordinates, is it last circle in the map or not.
frameCircle :: Coords -> Integer -> Bool
frameCircle (x, y) radius = ceiling (sqrt((abs x )^2 + (abs y)^2)) >= radius


-- | Convert from data Colors to standart Color
colors :: Colors -> Color
colors Red   = red
colors Blue  = blue
colors Green = green
colors Pink  = pink


-- | Function decides to open door or not.
openDoor :: Colors -> [Color] -> Tile
openDoor c cs 
  | oneOf (colors c) cs = Floor
  | otherwise = Door c


-- | Check is a color contains in a list of colors or not.
oneOf :: Color -> [Color] -> Bool
oneOf c cs = c `elem` cs


-- | Render a single tile given its type in data.
drawTile :: Tile -> Picture
drawTile Floor      = floorTile
drawTile Wall       = wallTile
drawTile Exit       = exitTile
drawTile (Button c) = buttonTile (colors c)
drawTile (Door c)   = doorTile (colors c)



-- | First tile map of the game.
tileMap1 :: State -> Tile
tileMap1 (State (x, y) _cs)
  | abs x > 4 || abs y > 4 = Wall
  | (x, y) == (3, 3)       = Exit
  | x == 1                 = Wall
  | x < 1 || y == -2       = Wall
  | otherwise              = Floor
 
-- | Second tile map of the game.
tileMap2 :: State -> Tile
tileMap2 (State (x, y) cs)
  | abs x < 2 && abs y < 2  = Exit
  | abs x > 9 || abs y > 9  = Wall
  | isEmpty door == False   = openDoor (findColors (x, y) doors)  cs
  | isEmpty button == False = Button (findColors (x, y) buttons)
  | circleByTiles (x, y) 2  = Wall
  | circleByTiles (x, y) 5  = Wall
  | circleByTiles (x, y) 8  = Wall
  | frameCircle (x, y) 11   = Wall
  | y == 0                  = Wall
  | x == 0 && y > -9        = Wall
  | otherwise               = Floor
  
  where
    door   = findTupel (x, y) doors
    button = findTupel (x, y) buttons


-- | Draw tile at the given coordinates.
drawTileAt :: Coords -> Tile -> Picture
drawTileAt (x, y) tile = translated x y (drawTile (tile))
    

-- | Render a series of values in a given range.
drawFromTo :: Coords -> (Double -> Picture) -> Picture
drawFromTo (from, to) render
  | from > to = blank
  | otherwise = render from <> drawFromTo (from + 1, to) render


-- | Draw a single row.
drawRow :: (State -> Tile) -> [Color] -> Coords -> Double -> Picture
drawRow tileMap cs (from, to) y = drawFromTo (from, to) drawRowTileAt
  where
    drawRowTileAt :: Double -> Picture
    drawRowTileAt x = drawTileAt (x, y) (tileMap (State (x, y) cs))


-- | Draw a rows.
drawRows :: (State -> Tile) -> [Color] -> Coords -> Coords -> Picture
drawRows tileMap cs (fromX, fromY) (toX, toY) 
    = drawFromTo (fromY, toY) (drawRow tileMap cs (fromX, toX))


-- | A picture of 'levelMap'.
drawLevelMap :: (State -> Tile) -> Double -> [Color] -> Picture
drawLevelMap tileMap size cs 
    = drawRows tileMap cs (-size, -size) (size, size)


-- | Take the next coordinates after checking in tryMove
nextCoord:: Dir -> Coords -> Coords
nextCoord R (x, y) = ((x + 1), y)
nextCoord L (x, y) = ((x - 1), y)
nextCoord U (x, y) = (x, (y + 1))
nextCoord D (x, y) = (x, (y - 1))


-- | Choose the state of actor
tryMove :: Dir -> State -> (State -> Tile) -> State
tryMove dir (State (x, y) cs) tileMap
  | canMove (tileMap (State (nextCoord dir (x, y)) cs)) == False = (State (x, y) cs)
  | otherwise = (State (nextCoord dir (x, y))  cs)


-- | Check if the actor can move to the next tile.
canMove :: Tile -> Bool
canMove Wall     = False
canMove (Door _) = False
canMove _        = True


-- | Find tupel in a list of (Color, Coords) by the give coordinates.
findTupel :: Coords -> [(Colors, Coords)] -> [(Colors, Coords)]
findTupel _coords [] = []
findTupel coords cs = filter (\e -> snd(e) == coords) cs


-- | Find color in a finded tupel.
findColors :: Coords -> [(Colors, Coords)] -> Colors
findColors coords cs = fst (head (findTupel coords cs))


-- | Remove the element from the list.
remove :: Eq a => a -> [a] -> [a]
remove elemet list = filter(\e -> e /= elemet) list


-- | Check if the list empty or not.
isEmpty :: [a] -> Bool
isEmpty = \list ->
  case list of
    [] -> True
    _  -> False
    
    
-- | Test is it button.
isButton :: Tile -> Bool
isButton (Button _) = True
isButton _          = False


-- | Switch color of tile.
colorOfTile :: Tile -> Color
colorOfTile (Button c) = colors c
colorOfTile (Door c)   = colors c


-- | Make 'activityOf' resettable on Esc.
withReset :: ActivityOf world -> ActivityOf world
withReset inputActivityOf = outputActivityOf
  where
  outputActivityOf = (\initState handleWorld renderWorld -> inputActivityOf initState (handleWorld' initState handleWorld) renderWorld)
    where 
      handleWorld' initState handleWorld (KeyPress "Esc") _  = initState
      handleWorld' initState handleWorld anyEvent state = handleWorld anyEvent state

-- | Add start screen to 'activityOf'.
withStartScreen :: ActivityOf (WithStartScreen world) -> ActivityOf world
withStartScreen inputActivityOf = outputActivityOf
  where
  outputActivityOf = (\initState handleWorld renderWorld -> inputActivityOf initState' (handleWorld' initState handleWorld) (renderWorld' renderWorld))
      where
        initState' = StartScreen

        handleWorld' initState handleWorld (KeyPress " " ) StartScreen = GameOn initState
        handleWorld' initState handleWorld _ StartScreen               = StartScreen
        handleWorld' initState handleWorld event (GameOn state)        = GameOn (handleWorld event state)

        renderWorld' renderWorld StartScreen  = startScreen
        renderWorld' renderWorld (GameOn state)  = renderWorld state
    


run :: IO ()
run = (withReset (withStartScreen tileGame)) initialWorld handleWorld renderWorld
    where 
      -- | Tile Game
      tileGame :: ActivityOf world
      tileGame initWorld handle render = activityOf initWorld handle render
      
      
      -- | Size of the next level
      initialSize :: Double
      initialSize = 21
    
    
      -- | Map of the game.
      initialMap :: (State -> Tile)
      initialMap = tileMap2
    
    
      -- | Start of the actor.
      initialWorld :: State
      initialWorld = State (-2, 9) []
      
      
      -- | Changing the world and position of the actor and the world
      handleWorld :: Event -> State -> State
      handleWorld (TimePassing dt) state   = (updateWorld dt state)
      handleWorld (KeyPress "Up") state    = tryMove U state initialMap
      handleWorld (KeyPress "Down") state  = tryMove D state initialMap
      handleWorld (KeyPress "Right") state = tryMove R state initialMap
      handleWorld (KeyPress "Left") state  = tryMove L state initialMap
      handleWorld (KeyPress "Enter") state = withOpenDoors state
      handleWorld _anyEvent state          = state
     
     
      -- | Render actor and world with new data.
      renderWorld :: State -> Picture
      renderWorld (State (x, y) cs) = actor <> levelMap
        where
         actor = translated x y (lettering "\x1F6B6") 
         levelMap  = (drawLevelMap initialMap initialSize cs )
      
      
      -- | Check if it is button then work with it color
      withOpenDoors :: State -> State
      withOpenDoors (State (x, y) cs)
        | (isButton (initialMap (State (x, y) cs))) == False = (State (x, y) cs)
        | otherwise = (State (x, y) (openOrClose (colorOfTile (initialMap (State (x, y) cs))) cs))
      
      
      -- | Close doors if their color is in a list and remove it from list, otherwise add color into the list
      openOrClose ::  Color -> [Color] -> [Color]
      openOrClose c cs 
        | oneOf c cs == True = remove c cs
        | otherwise          = c : cs
    
    
      -- | Update world by time.
      updateWorld :: Double -> State -> State
      updateWorld _dt = id

