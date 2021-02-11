module Types where

data Pattern = Pattern
  { name :: String -- ^ some description of this pattern. used by C code in the compiler, so needs to be a valid C identifier. ignored by simulator.
  , initial :: [Color] -- ^ an initial list of colors that you want to show up as the first "frame" on your lights
  , step :: Transform -- ^ see below. a Transform is a way to go from the current list of light colors, to the next, as a single step
  , millisecondsPerStep :: Int -- ^ how long to sleep between steps, in milliseconds
  , numSteps :: Int -- ^ how many steps to run a pattern for
  }

-- colors are a slightly-better-named tuple of red, green, and blue components, ranging from 0 to 255.
data Color = Color { r :: Int, g :: Int, b :: Int }
  deriving Eq

data Transform -- Transforms act on [Color]
  = Rotate Int -- rotates array by that number of pixels
  | Brightness Int -- takes an array and brightens each member of it
  | Hue Int -- cycles through colors (runs through hues on color wheel) by a given amount
  | Invert -- flips colors to their opposites
  | Compose [Transform] -- applies multiple transforms
  | AddColor Color -- adds color to each pixel
  | Set [Color] -- sets to new given LightArray
  | SetAll Color -- sets all pixels to new given color
  | IfFirstPixel (Color -> Bool) Transform Transform -- if statements taking a predicate, a Transform to run if the predicate is true, and a Transform to run if it's false
                                                    -- because IfFirstPixel takes a Haskell function as an argument, itcurrently only works in simulator
  | NoOp -- does nothing, which is surprisingly useful, sometimes
