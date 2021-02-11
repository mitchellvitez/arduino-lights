module Patterns where

import Color
import Types

-- this is where you should write patterns to run on your own lights

patterns :: [Pattern]
patterns = [cycleRedGreenBlue, invertStartYellow, theaterMarquee, increaseBrightness, fadeRedGreen]

cycleRedGreenBlue :: Pattern
cycleRedGreenBlue = Pattern
  { name = "cycleRedGreenBlue"
  , initial = repeat red
  , step =
      IfFirstPixel (==red)   (SetAll green) $
      IfFirstPixel (==green) (SetAll blue) $
      IfFirstPixel (==blue)  (SetAll red) NoOp
  , millisecondsPerStep = 750
  , numSteps = 12
  }

increaseBrightness :: Pattern
increaseBrightness = Pattern
  { name = "fadeBlackWhite"
  , initial = repeat black
  , step = Brightness 5
  , millisecondsPerStep = 100
  , numSteps = 50
  }

fadeRedGreen :: Pattern
fadeRedGreen = Pattern
  { name = "fadeRedGreen"
  , initial = repeat red
  , step = AddColor (Color (-6) 6 0)
  , millisecondsPerStep = 100
  , numSteps = 50
  }

cycleHue :: Pattern
cycleHue = Pattern
  { name = "cycleHue"
  , initial = repeat $ Color 25 25 25
  , step = Hue 10
  , millisecondsPerStep = 100
  , numSteps = 500
  }

invertStartYellow :: Pattern
invertStartYellow = Pattern
  { name = "invertStartYellow"
  , initial = repeat yellow
  , step = Invert
  , millisecondsPerStep = 2000
  , numSteps = 5
  }

theaterMarquee :: Pattern
theaterMarquee = Pattern
  { name = "theaterMarquee"
  , initial = cycle [black, black, white]
  , step = Rotate 1
  , millisecondsPerStep = 300
  , numSteps = 20
  }
