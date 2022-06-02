{-# LANGUAGE OverloadedStrings #-}
module Project where

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Color -> Double -> Double -> Picture
lightCircle c dx dy = translated dx dy (colored c (solidCircle 1))


-- | Red traffic light.
redLight :: Double -> Double-> Picture
redLight dx dy = lightCircle red dx dy


-- | Yellow traffic light.
yellowLight :: Double -> Double -> Picture
yellowLight dx dy = lightCircle yellow dx dy


-- | Green traffic light.
greenLight :: Double -> Double -> Picture
greenLight dx dy = lightCircle green dx dy


-- | Dark red for frame background.
darkRedFrameCircle :: Double -> Double -> Picture
darkRedFrameCircle dx dy = lightCircle (dark (dark red)) dx dy


-- | Dark yellow for frame background.
darkYellowFrameCircle :: Double -> Double -> Picture
darkYellowFrameCircle dx dy = lightCircle (dark (dark yellow)) dx dy


-- | Dark green for frame background.
darkGreenFrameCircle :: Double -> Double -> Picture
darkGreenFrameCircle dx dy = lightCircle (dark (dark green)) dx dy


-- | Two dark lights on a traffic light frame.
basicCirclesForFrameTwo :: Double -> Double -> Picture
basicCirclesForFrameTwo dx dy = darkRedFrameCircle dx (dy + 1.2) <> darkGreenFrameCircle dx (dy - 1.2)


-- | Three dark lights on a traffic light frame.
basicCirclesForFrameFour :: Double -> Double -> Picture
basicCirclesForFrameFour dx dy =  darkRedFrameCircle dx (dy + 2.4) <> darkGreenFrameCircle dx (dy - 2.4) <> darkYellowFrameCircle dx dy


-- | Triangle for Traffic Light
tiangleForTrafficLight :: Double -> Double -> Double -> Double -> Picture
tiangleForTrafficLight width height dx dy = colored grey (thickPolygon 0.5[((dx - width/2 + 0.25), (dy + height/2 - 0.5)),((dx - width/2 + 0.25), (dy + height/2 - 0.25)),((dx - width/2), (dy + height/2 - 0.25))])


-- | Traffic Light Post
trafficLightPost :: Double -> Double -> Double -> Double -> Picture
trafficLightPost width height dx dy = translated dx dy (colored black (solidRectangle width height))


-- | Default traffic light frame.
frame :: Double -> Double -> Double -> Double -> Color -> Picture
frame width height dx dy c = translated dx dy (colored c (solidRectangle width height)) 


-- | Frame for traffic light with two colors.
frameTwo :: Double -> Double -> Double -> Double -> Picture
frameTwo width height dx dy = basicCirclesForFrameTwo dx dy <> frame width height dx dy black <> frame (width + 0.5) (height + 0.5) dx dy grey  <> trafficLightPost 0.3 5 (dx) (dy - 5)


-- | Frame for traffic light with three colors.
frameFour :: Double -> Double -> Double -> Double -> Picture
frameFour width height dx dy = frame width height (dx + 0.5) dy grey <> tiangleForTrafficLight width height dx dy <> trafficLightPost 0.3 5 (dx + 0.5) (dy -5)


-- | Blinking green light
blinkingGreen :: Double -> Double -> Double -> Picture
blinkingGreen t dx dy
  | floor (t * 10) `mod` 2 == 0 = trafficLightsTwo True dx dy
  | otherwise                   = frameTwo 2.5 5 dx dy


-- | Choosing the state of the green color
greenColor :: Double -> Double -> Double -> Picture
greenColor t dx dy
  | floor (t + 1) `mod` 4 == 0 = blinkingGreen t dx dy
  | otherwise                  = trafficLightsTwo True dx dy


-- | Simple traffic lights with two states.
--
-- * 'True' — green light
-- * 'False' — red light
trafficLightsTwo :: Bool -> Double -> Double -> Picture
trafficLightsTwo True dx dy  = greenLight dx (dy - 1.2) <> frameTwo 2.5 5 dx dy
trafficLightsTwo False dx dy = redLight dx (dy + 1.2) <> frameTwo 2.5 5 dx dy  


-- | Simple traffic light with Four states.
--
-- * '0' - green light active 
-- * '1' - yellow light active
-- * '2' - red light active
-- * '3' - red and yellow lights active
trafficLightsFour :: Integer -> Double -> Double -> Picture
trafficLightsFour state dx dy
   | state == 0 = frameFour 1.25 7.5 dx dy <> greenLight dx (dy - 2.4) <> basicCirclesForFrameFour dx dy
   | state == 1 = frameFour 1.25 7.5 dx dy <> yellowLight dx dy <> basicCirclesForFrameFour dx dy
   | state == 2 = frameFour 1.25 7.5 dx dy <> redLight dx (dy + 2.4) <> basicCirclesForFrameFour dx dy
   | state == 3 = frameFour 1.25 7.5 dx dy <> yellowLight dx dy <> redLight dx (dy + 2.4) <> basicCirclesForFrameFour dx dy
   | otherwise = blank


-- | Traffic lights controller switching lights every 4 seconds and has blinking green light before red light.
trafficControllerTwo :: Double -> Double -> Double -> Picture
trafficControllerTwo dx dy t
  | floor (t / 4) `mod` 2 == 0 = greenColor t dx dy
  | otherwise                  = trafficLightsTwo False dx dy


-- | Traffic lights controller has time slots:
--  
-- * 3 seconds on green light
-- * 1 second on yellow light
-- * 3 seconds on red light
-- * 1 second on red & yellow lights
trafficControllerFour :: Double -> Double -> Double -> Picture
trafficControllerFour dx dy t
  | floor (t) `mod` 8 == 0     = trafficLightsFour 1 dx dy
  | floor (t) `mod` 4 == 0     = trafficLightsFour 3 dx dy
  | floor (t / 4) `mod` 2 == 0 = trafficLightsFour 2 dx dy
  | otherwise                  = trafficLightsFour 0 dx dy

crossWalkForCyclists :: Picture
crossWalkForCyclists = frame 8 15 0 0 blue 

-- | Picture of crosswalk on the road
crossWalkForPedestrians :: Picture
crossWalkForPedestrians = frame 10 2 12 (-6) yellow <> frame 10 2 12 (-3) yellow <> frame 10 2 12 0 yellow <> frame 10 2 12 3 yellow <> frame 10 2 12 6 yellow


-- | Line of the road
lineOfRoad :: Picture
lineOfRoad = frame 15 0.3 (-28) 0 white <> frame 0.3 15 (-20.5) 0 white 


-- | Road
road :: Double -> Picture
road n = crossWalkForPedestrians <> crossWalkForCyclists <> lineOfRoad <> rotated (pi) (lineOfRoad)<> frame 70 15 0 0 black <> frame 70 17 0 0 grey


run :: IO ()
run = do 
  animationOf (road <>trafficControllerTwo 19 15 <> trafficControllerTwo 5 15 <>trafficControllerFour (-19) 15 )

