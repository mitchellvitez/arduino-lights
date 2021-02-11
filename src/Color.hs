module Color where

import Types (Color(..))

-- if you have terminal colors working and want to use the same symbol for each light, use `lightSymbol = const "X"`
lightSymbol :: Color -> String
lightSymbol c
  | c == red    = "R"
  | c == green  = "G"
  | c == blue   = "B"
  | c == yellow = "Y"
  | c == black  = "_"
  | c == white  = "*"
  | otherwise   = "o"

white        = Color 255 255 255
black        = Color   0   0   0
red          = Color 255   0   0
green        = Color   0 255   0
blue         = Color   0   0 255
yellow       = Color 255 255   0
cyan         = Color   0 255 255
pink         = Color 255   0 255
magenta      = Color 255   0 255
darkRed      = Color 127   0   0
darkGreen    = Color   0 127   0
darkBlue     = Color   0   0 127
darkYellow   = Color 127 127   0
darkCyan     = Color   0 127 127
darkPink     = Color 127   0 127
gray         = Color 127 127 127
grey         = Color 127 127 127
lightRed     = Color 255 127 127
lightGreen   = Color 127 255 127
lightBlue    = Color 127 127 255
lightYellow  = Color 255 255 127
lightPink    = Color 255 127 255
lightMagenta = Color 255 125 255
lightCyan    = Color 127 255 255
orange       = Color 255 127   0
purple       = Color  64   0 255
salmon       = Color 255   0 127
aquamarine   = Color   0 255 127
lime         = Color 127 255   0
aubergine    = Color 127   0 255
cerulean     = Color   0 127 255

