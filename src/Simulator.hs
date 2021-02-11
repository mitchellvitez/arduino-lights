{-# LANGUAGE RecordWildCards #-}

module Simulator where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.List (intercalate)

import Color
import Config
import Types

simulate :: [Pattern] -> IO ()
simulate patterns = mapM_ simulatePattern $ cycle patterns

simulatePattern :: Pattern -> IO ()
simulatePattern Pattern{..} = do
  -- putStrLn $ "running " <> name <> " pattern"
  let allSteps = take numSteps $ iterate (simulateStep step) initial
  forM_ allSteps $ \arr -> do
    simulateShow arr
    threadDelay (millisecondsPerStep * 1000) -- threadDelay uses microseconds

simulateStep :: Transform -> [Color] -> [Color]
simulateStep transform arr = case transform of
  Rotate n -> take numLights $ drop n $ cycle arr
  Brightness n -> map (\(Color r g b) -> Color (r+n) (g+n) (b+n)) arr
  Invert -> map (\(Color r g b) -> Color (255-r) (255-g) (255-b)) arr
  Compose ts -> concatMap (flip simulateStep arr) ts
  AddColor (Color r g b) -> map (\(Color r2 g2 b2) -> Color (r+r2) (g+g2) (b+b2)) arr
  Set colors -> colors
  SetAll color -> take numLights $ repeat color
  IfFirstPixel pred t t2 -> if pred (head arr) then simulateStep t arr else simulateStep t2 arr
  Hue n -> arr -- currently compiler-only
  NoOp -> arr

simulateShow :: [Color] -> IO ()
simulateShow colors = do
  let arr = take numLights $ cycle colors
      -- use this if terminal colors are working
      toLight c@(Color r g b) = "\x1b[38;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m" <> lightSymbol c

      -- use this if terminal colors aren't working
      -- toLight = lightSymbol

  mapM_ (putStr . toLight) arr -- print a character in the correct color for each color
  putStrLn ""

clamp :: Color -> Color
clamp (Color r g b) = Color
  { r = clampOne r
  , g = clampOne g
  , b = clampOne b
  }
  where clampOne n = if n < 0 then 0 else if n > 255 then 255 else n
