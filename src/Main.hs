module Main where

import Compiler (compile)
import Patterns (patterns)
import Simulator (simulate)

main :: IO ()
main = do
  -- writeFile "./ArduinoLights/ArduinoLights.ino" $ compile patterns -- run compiler
  simulate patterns -- run simulator
