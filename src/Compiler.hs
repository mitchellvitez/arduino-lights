{-# LANGUAGE RecordWildCards #-}

module Compiler where

import Data.List (intercalate)

import Config
import Types

-- generate C code that can run on arduino
-- this is not the recommended way to write a compiler, normally you should work with something nicer than strings
-- but, this was quick to write and it's not generating too much code
compile :: [Pattern] -> String
compile patterns = intercalate "\n" $
  [ "#include <Adafruit_NeoPixel.h>"
  , ""
  , "#define LED_PIN " <> show pin
  , "#define LED_COUNT " <> show numLights
  , "#define BRIGHTNESS " <> show brightness
  , ""
  , "Adafruit_NeoPixel strip(LED_COUNT, LED_PIN, NEO_GRB + NEO_KHZ800);"
  , ""
  , ""
  , "int r (int i) {"
  , "  return (strip.getPixelColor(i) >> 16) & 0x7f;"
  , "}"
  , "int g (int i) {"
  , "  return (strip.getPixelColor(i) >>  8) & 0x7f;"
  , "}"
  , "int b (int i) {"
  , "  return (strip.getPixelColor(i)) & 0x7f;"
  , "}"
  , ""
  , "int clamp(int n) {"
  , "  if (n > 255) { return 255; }"
  , "  if (n < 0) { return 0; }"
  , "  return n;"
  , "}"
  , ""
  , "void brighten(uint8_t n) {"
  , "  for (int i = 0; i < strip.numPixels(); ++i) {"
  , "    strip.setPixelColor(i, strip.Color("
  , "      clamp(r(i) + n),"
  , "      clamp(g(i) + n),"
  , "      clamp(b(i) + n)"
  , "    ));"
  , "  }"
  , "}"
  , ""
  , "void invert() {"
  , "  for (int i = 0; i < strip.numPixels(); ++i) {"
  , "    strip.setPixelColor(i, strip.Color("
  , "      clamp(255 - r(i)),"
  , "      clamp(255 - g(i)),"
  , "      clamp(255 - b(i))"
  , "    ));"
  , "  }"
  , "}"
  , ""
  , "void rotate(uint8_t n) {"
  , "  for(int i = 0; i < n; ++i) {"
  , "    uint32_t tmp = strip.getPixelColor(0);"
  , "    for(int j = 0; j < strip.numPixels() - 1; ++j) {"
  , "       strip.setPixelColor(j, strip.getPixelColor(j + 1));"
  , "    }"
  , "    strip.setPixelColor(strip.numPixels() - 1, tmp);"
  , "  }"
  , "}"
  , ""
  , "void addColor(uint8_t r2, uint8_t g2, uint8_t b2) {"
  , "  for(int i = 0; i < strip.numPixels(); ++i) {"
  , "    strip.setPixelColor(i, strip.Color("
  , "      clamp(r(i) + r2),"
  , "      clamp(g(i) + g2),"
  , "      clamp(b(i) + b2)"
  , "    ));"
  , "  }"
  , "}"
  , ""
  , "uint8_t updateHue(uint8_t *r, uint8_t *g, uint8_t *b, int n) {"
  , "  // take r g b and turn into h s v"
  , "  uint8_t M = max(*r, max(*g, *b));"
  , "  uint8_t m = min(*r, max(*g, *b));"
  , "  uint8_t c = M - m;"
  , "  double v = M;"
  , "  double s = c / v;"
  , "  int h;"
  , "  if (c == 0) {"
  , "    h = 0;"
  , "  } else {"
  , "    if (*r == M) {"
  , "      h = (((*g - *b) / c) % 6) * 60;"
  , "    } else if (*g == M) {"
  , "      h = (((*b - *r) / c) + 2) * 60;"
  , "    } else {"
  , "      h = (((*r - *g) / c) + 4) * 60;"
  , "    }"
  , "  }"
  , "  // do the addition"
  , "  h = (h + n) % 360;"
  , "  // take hsv and turn back into r g b"
  , "  uint8_t x = c * (1 - abs(((h / 60) % 2) - 1));"
  , "  if (h < 0) {"
  , "    *r = *g = *b = m;"
  , "  } else if (h < 61) {"
  , "    *r = c + m;"
  , "    *g = x + m;"
  , "    *b = m;"
  , "  } else if (h < 121) {"
  , "    *r = x + m;"
  , "    *g = c + m;"
  , "    *b = m;"
  , "  } else if (h < 181) {"
  , "    *r = m;"
  , "    *g = c + m;"
  , "    *b = x + m;"
  , "  } else if (h < 241) {"
  , "    *r = m;"
  , "    *g = x + m;"
  , "    *b = c + m;"
  , "  } else if (h < 301) {"
  , "    *r = x + m;"
  , "    *g = m;"
  , "    *b = c + m;"
  , "  } else if (h < 361) {"
  , "    *r = c + m;"
  , "    *g = m;"
  , "    *b = x + m;"
  , "  } else {"
  , "    *r = *g = *b = m;"
  , "  }"
  , "}"
  , ""
  , "void hue(int n) {"
  , "  for(int i = 0; i < strip.numPixels(); ++i) {"
  , "    uint8_t r, g, b;"
  , "    updateHue(&r, &g, &b, n);"
  , "    strip.setPixelColor(i, strip.Color("
  , "      clamp(r),"
  , "      clamp(g),"
  , "      clamp(b)"
  , "    ));"
  , "  }"
  , "}"
  , ""
  , "void setup() {"
  , "  strip.begin();"
  , "  strip.show();"
  , "  strip.setBrightness(BRIGHTNESS);"
  , "}"
  , ""
  , "void loop() {"
  , "" <> intercalate "\n" (map compileLoop patterns)
  , "}\n"
  , "" <> intercalate "\n" (map compilePattern patterns)
  ]

compileLoop :: Pattern -> String
compileLoop Pattern{..} =
  "  " <> name <> "();\n  strip.clear();"

compilePattern :: Pattern -> String
compilePattern Pattern{..} = intercalate "\n"
  [ "void " <> name <> "() {"
  , "" <> compileInitial initial
  , "  strip.show();"
  , "  delay(" <> show millisecondsPerStep <> ");"
  , "  for (int i = 0; i < " <> show numSteps <> "; ++i) {"
  , "    " <> compileTransform step
  , "    strip.show();"
  , "    delay(" <> show millisecondsPerStep <> ");"
  , "  }"
  , "}"
  ]

compileInitial :: [Color] -> String
compileInitial initial = intercalate "\n" $ map line [0..numLights-1]
  -- TODO: make this generate better code when all colors in initialValid are the same
  where line i = "  strip.setPixelColor(" <> show i <> ", " <> compileColor (initialValid !! i) <> ");"
        initialValid = take numLights $ cycle initial

compileColor :: Color -> String
compileColor Color{..} = "strip.Color(" <> show r <> ", " <> show g <> ", " <> show b <> ")"

compileTransform :: Transform -> String
compileTransform transform = case transform of
  Rotate n -> "rotate(" <> show n <> ");"
  Brightness n -> "brighten(" <> show n <> ");"
  Hue n -> "hue(" <> show n <> ");"
  Invert -> "invert();"
  Compose ts -> intercalate "\n" $ map compileTransform ts
  AddColor (Color r g b) -> "addColor(" <> show r <> ", " <> show g <> ", " <> show b <> ");"
  Set colors -> compileInitial colors
  SetAll color -> compileInitial $ replicate numLights color
  IfFirstPixel _ _ _ -> "" -- currently simulator-only
  NoOp -> ""
