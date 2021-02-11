module Config where

-- which pin on the arduino should we send data on
pin :: Int
pin = 5

-- number of individual "pixels" you have on your RGB LED strand
-- recommend 300 for hardware, 80 for software
numLights :: Int
numLights = 80

-- 0 to 255, lower values will draw less current on the lights
brightness :: Int
brightness = 50
