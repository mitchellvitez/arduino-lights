#include <Adafruit_NeoPixel.h>

#define LED_PIN 5
#define LED_COUNT 80
#define BRIGHTNESS 50

Adafruit_NeoPixel strip(LED_COUNT, LED_PIN, NEO_GRB + NEO_KHZ800);


int r (int i) {
  return (strip.getPixelColor(i) >> 16) & 0x7f;
}
int g (int i) {
  return (strip.getPixelColor(i) >>  8) & 0x7f;
}
int b (int i) {
  return (strip.getPixelColor(i)) & 0x7f;
}

int clamp(int n) {
  if (n > 255) { return 255; }
  if (n < 0) { return 0; }
  return n;
}

void brighten(uint8_t n) {
  for (int i = 0; i < strip.numPixels(); ++i) {
    strip.setPixelColor(i, strip.Color(
      clamp(r(i) + n),
      clamp(g(i) + n),
      clamp(b(i) + n)
    ));
  }
}

void invert() {
  for (int i = 0; i < strip.numPixels(); ++i) {
    strip.setPixelColor(i, strip.Color(
      clamp(255 - r(i)),
      clamp(255 - g(i)),
      clamp(255 - b(i))
    ));
  }
}

void rotate(uint8_t n) {
  for(int i = 0; i < n; ++i) {
    uint32_t tmp = strip.getPixelColor(0);
    for(int j = 0; j < strip.numPixels() - 1; ++j) {
       strip.setPixelColor(j, strip.getPixelColor(j + 1));
    }
    strip.setPixelColor(strip.numPixels() - 1, tmp);
  }
}

void addColor(uint8_t r2, uint8_t g2, uint8_t b2) {
  for(int i = 0; i < strip.numPixels(); ++i) {
    strip.setPixelColor(i, strip.Color(
      clamp(r(i) + r2),
      clamp(g(i) + g2),
      clamp(b(i) + b2)
    ));
  }
}

uint8_t updateHue(uint8_t *r, uint8_t *g, uint8_t *b, int n) {
  // take r g b and turn into h s v
  uint8_t M = max(*r, max(*g, *b));
  uint8_t m = min(*r, max(*g, *b));
  uint8_t c = M - m;
  double v = M;
  double s = c / v;
  int h;
  if (c == 0) {
    h = 0;
  } else {
    if (*r == M) {
      h = (((*g - *b) / c) % 6) * 60;
    } else if (*g == M) {
      h = (((*b - *r) / c) + 2) * 60;
    } else {
      h = (((*r - *g) / c) + 4) * 60;
    }
  }
  // do the addition
  h = (h + n) % 360;
  // take hsv and turn back into r g b
  uint8_t x = c * (1 - abs(((h / 60) % 2) - 1));
  if (h < 0) {
    *r = *g = *b = m;
  } else if (h < 61) {
    *r = c + m;
    *g = x + m;
    *b = m;
  } else if (h < 121) {
    *r = x + m;
    *g = c + m;
    *b = m;
  } else if (h < 181) {
    *r = m;
    *g = c + m;
    *b = x + m;
  } else if (h < 241) {
    *r = m;
    *g = x + m;
    *b = c + m;
  } else if (h < 301) {
    *r = x + m;
    *g = m;
    *b = c + m;
  } else if (h < 361) {
    *r = c + m;
    *g = m;
    *b = x + m;
  } else {
    *r = *g = *b = m;
  }
}

void hue(int n) {
  for(int i = 0; i < strip.numPixels(); ++i) {
    uint8_t r, g, b;
    updateHue(&r, &g, &b, n);
    strip.setPixelColor(i, strip.Color(
      clamp(r),
      clamp(g),
      clamp(b)
    ));
  }
}

void setup() {
  strip.begin();
  strip.show();
  strip.setBrightness(BRIGHTNESS);
}

void loop() {
  cycleRedGreenBlue();
  strip.clear();
  invertStartYellow();
  strip.clear();
  theaterMarquee();
  strip.clear();
  fadeBlackWhite();
  strip.clear();
  fadeRedGreen();
  strip.clear();
}

void cycleRedGreenBlue() {
  strip.setPixelColor(0, strip.Color(255, 0, 0));
  strip.setPixelColor(1, strip.Color(255, 0, 0));
  strip.setPixelColor(2, strip.Color(255, 0, 0));
  strip.setPixelColor(3, strip.Color(255, 0, 0));
  strip.setPixelColor(4, strip.Color(255, 0, 0));
  strip.setPixelColor(5, strip.Color(255, 0, 0));
  strip.setPixelColor(6, strip.Color(255, 0, 0));
  strip.setPixelColor(7, strip.Color(255, 0, 0));
  strip.setPixelColor(8, strip.Color(255, 0, 0));
  strip.setPixelColor(9, strip.Color(255, 0, 0));
  strip.setPixelColor(10, strip.Color(255, 0, 0));
  strip.setPixelColor(11, strip.Color(255, 0, 0));
  strip.setPixelColor(12, strip.Color(255, 0, 0));
  strip.setPixelColor(13, strip.Color(255, 0, 0));
  strip.setPixelColor(14, strip.Color(255, 0, 0));
  strip.setPixelColor(15, strip.Color(255, 0, 0));
  strip.setPixelColor(16, strip.Color(255, 0, 0));
  strip.setPixelColor(17, strip.Color(255, 0, 0));
  strip.setPixelColor(18, strip.Color(255, 0, 0));
  strip.setPixelColor(19, strip.Color(255, 0, 0));
  strip.setPixelColor(20, strip.Color(255, 0, 0));
  strip.setPixelColor(21, strip.Color(255, 0, 0));
  strip.setPixelColor(22, strip.Color(255, 0, 0));
  strip.setPixelColor(23, strip.Color(255, 0, 0));
  strip.setPixelColor(24, strip.Color(255, 0, 0));
  strip.setPixelColor(25, strip.Color(255, 0, 0));
  strip.setPixelColor(26, strip.Color(255, 0, 0));
  strip.setPixelColor(27, strip.Color(255, 0, 0));
  strip.setPixelColor(28, strip.Color(255, 0, 0));
  strip.setPixelColor(29, strip.Color(255, 0, 0));
  strip.setPixelColor(30, strip.Color(255, 0, 0));
  strip.setPixelColor(31, strip.Color(255, 0, 0));
  strip.setPixelColor(32, strip.Color(255, 0, 0));
  strip.setPixelColor(33, strip.Color(255, 0, 0));
  strip.setPixelColor(34, strip.Color(255, 0, 0));
  strip.setPixelColor(35, strip.Color(255, 0, 0));
  strip.setPixelColor(36, strip.Color(255, 0, 0));
  strip.setPixelColor(37, strip.Color(255, 0, 0));
  strip.setPixelColor(38, strip.Color(255, 0, 0));
  strip.setPixelColor(39, strip.Color(255, 0, 0));
  strip.setPixelColor(40, strip.Color(255, 0, 0));
  strip.setPixelColor(41, strip.Color(255, 0, 0));
  strip.setPixelColor(42, strip.Color(255, 0, 0));
  strip.setPixelColor(43, strip.Color(255, 0, 0));
  strip.setPixelColor(44, strip.Color(255, 0, 0));
  strip.setPixelColor(45, strip.Color(255, 0, 0));
  strip.setPixelColor(46, strip.Color(255, 0, 0));
  strip.setPixelColor(47, strip.Color(255, 0, 0));
  strip.setPixelColor(48, strip.Color(255, 0, 0));
  strip.setPixelColor(49, strip.Color(255, 0, 0));
  strip.setPixelColor(50, strip.Color(255, 0, 0));
  strip.setPixelColor(51, strip.Color(255, 0, 0));
  strip.setPixelColor(52, strip.Color(255, 0, 0));
  strip.setPixelColor(53, strip.Color(255, 0, 0));
  strip.setPixelColor(54, strip.Color(255, 0, 0));
  strip.setPixelColor(55, strip.Color(255, 0, 0));
  strip.setPixelColor(56, strip.Color(255, 0, 0));
  strip.setPixelColor(57, strip.Color(255, 0, 0));
  strip.setPixelColor(58, strip.Color(255, 0, 0));
  strip.setPixelColor(59, strip.Color(255, 0, 0));
  strip.setPixelColor(60, strip.Color(255, 0, 0));
  strip.setPixelColor(61, strip.Color(255, 0, 0));
  strip.setPixelColor(62, strip.Color(255, 0, 0));
  strip.setPixelColor(63, strip.Color(255, 0, 0));
  strip.setPixelColor(64, strip.Color(255, 0, 0));
  strip.setPixelColor(65, strip.Color(255, 0, 0));
  strip.setPixelColor(66, strip.Color(255, 0, 0));
  strip.setPixelColor(67, strip.Color(255, 0, 0));
  strip.setPixelColor(68, strip.Color(255, 0, 0));
  strip.setPixelColor(69, strip.Color(255, 0, 0));
  strip.setPixelColor(70, strip.Color(255, 0, 0));
  strip.setPixelColor(71, strip.Color(255, 0, 0));
  strip.setPixelColor(72, strip.Color(255, 0, 0));
  strip.setPixelColor(73, strip.Color(255, 0, 0));
  strip.setPixelColor(74, strip.Color(255, 0, 0));
  strip.setPixelColor(75, strip.Color(255, 0, 0));
  strip.setPixelColor(76, strip.Color(255, 0, 0));
  strip.setPixelColor(77, strip.Color(255, 0, 0));
  strip.setPixelColor(78, strip.Color(255, 0, 0));
  strip.setPixelColor(79, strip.Color(255, 0, 0));
  strip.show();
  delay(750);
  for (int i = 0; i < 12; ++i) {
    
    strip.show();
    delay(750);
  }
}
void invertStartYellow() {
  strip.setPixelColor(0, strip.Color(255, 255, 0));
  strip.setPixelColor(1, strip.Color(255, 255, 0));
  strip.setPixelColor(2, strip.Color(255, 255, 0));
  strip.setPixelColor(3, strip.Color(255, 255, 0));
  strip.setPixelColor(4, strip.Color(255, 255, 0));
  strip.setPixelColor(5, strip.Color(255, 255, 0));
  strip.setPixelColor(6, strip.Color(255, 255, 0));
  strip.setPixelColor(7, strip.Color(255, 255, 0));
  strip.setPixelColor(8, strip.Color(255, 255, 0));
  strip.setPixelColor(9, strip.Color(255, 255, 0));
  strip.setPixelColor(10, strip.Color(255, 255, 0));
  strip.setPixelColor(11, strip.Color(255, 255, 0));
  strip.setPixelColor(12, strip.Color(255, 255, 0));
  strip.setPixelColor(13, strip.Color(255, 255, 0));
  strip.setPixelColor(14, strip.Color(255, 255, 0));
  strip.setPixelColor(15, strip.Color(255, 255, 0));
  strip.setPixelColor(16, strip.Color(255, 255, 0));
  strip.setPixelColor(17, strip.Color(255, 255, 0));
  strip.setPixelColor(18, strip.Color(255, 255, 0));
  strip.setPixelColor(19, strip.Color(255, 255, 0));
  strip.setPixelColor(20, strip.Color(255, 255, 0));
  strip.setPixelColor(21, strip.Color(255, 255, 0));
  strip.setPixelColor(22, strip.Color(255, 255, 0));
  strip.setPixelColor(23, strip.Color(255, 255, 0));
  strip.setPixelColor(24, strip.Color(255, 255, 0));
  strip.setPixelColor(25, strip.Color(255, 255, 0));
  strip.setPixelColor(26, strip.Color(255, 255, 0));
  strip.setPixelColor(27, strip.Color(255, 255, 0));
  strip.setPixelColor(28, strip.Color(255, 255, 0));
  strip.setPixelColor(29, strip.Color(255, 255, 0));
  strip.setPixelColor(30, strip.Color(255, 255, 0));
  strip.setPixelColor(31, strip.Color(255, 255, 0));
  strip.setPixelColor(32, strip.Color(255, 255, 0));
  strip.setPixelColor(33, strip.Color(255, 255, 0));
  strip.setPixelColor(34, strip.Color(255, 255, 0));
  strip.setPixelColor(35, strip.Color(255, 255, 0));
  strip.setPixelColor(36, strip.Color(255, 255, 0));
  strip.setPixelColor(37, strip.Color(255, 255, 0));
  strip.setPixelColor(38, strip.Color(255, 255, 0));
  strip.setPixelColor(39, strip.Color(255, 255, 0));
  strip.setPixelColor(40, strip.Color(255, 255, 0));
  strip.setPixelColor(41, strip.Color(255, 255, 0));
  strip.setPixelColor(42, strip.Color(255, 255, 0));
  strip.setPixelColor(43, strip.Color(255, 255, 0));
  strip.setPixelColor(44, strip.Color(255, 255, 0));
  strip.setPixelColor(45, strip.Color(255, 255, 0));
  strip.setPixelColor(46, strip.Color(255, 255, 0));
  strip.setPixelColor(47, strip.Color(255, 255, 0));
  strip.setPixelColor(48, strip.Color(255, 255, 0));
  strip.setPixelColor(49, strip.Color(255, 255, 0));
  strip.setPixelColor(50, strip.Color(255, 255, 0));
  strip.setPixelColor(51, strip.Color(255, 255, 0));
  strip.setPixelColor(52, strip.Color(255, 255, 0));
  strip.setPixelColor(53, strip.Color(255, 255, 0));
  strip.setPixelColor(54, strip.Color(255, 255, 0));
  strip.setPixelColor(55, strip.Color(255, 255, 0));
  strip.setPixelColor(56, strip.Color(255, 255, 0));
  strip.setPixelColor(57, strip.Color(255, 255, 0));
  strip.setPixelColor(58, strip.Color(255, 255, 0));
  strip.setPixelColor(59, strip.Color(255, 255, 0));
  strip.setPixelColor(60, strip.Color(255, 255, 0));
  strip.setPixelColor(61, strip.Color(255, 255, 0));
  strip.setPixelColor(62, strip.Color(255, 255, 0));
  strip.setPixelColor(63, strip.Color(255, 255, 0));
  strip.setPixelColor(64, strip.Color(255, 255, 0));
  strip.setPixelColor(65, strip.Color(255, 255, 0));
  strip.setPixelColor(66, strip.Color(255, 255, 0));
  strip.setPixelColor(67, strip.Color(255, 255, 0));
  strip.setPixelColor(68, strip.Color(255, 255, 0));
  strip.setPixelColor(69, strip.Color(255, 255, 0));
  strip.setPixelColor(70, strip.Color(255, 255, 0));
  strip.setPixelColor(71, strip.Color(255, 255, 0));
  strip.setPixelColor(72, strip.Color(255, 255, 0));
  strip.setPixelColor(73, strip.Color(255, 255, 0));
  strip.setPixelColor(74, strip.Color(255, 255, 0));
  strip.setPixelColor(75, strip.Color(255, 255, 0));
  strip.setPixelColor(76, strip.Color(255, 255, 0));
  strip.setPixelColor(77, strip.Color(255, 255, 0));
  strip.setPixelColor(78, strip.Color(255, 255, 0));
  strip.setPixelColor(79, strip.Color(255, 255, 0));
  strip.show();
  delay(2000);
  for (int i = 0; i < 5; ++i) {
    invert();
    strip.show();
    delay(2000);
  }
}
void theaterMarquee() {
  strip.setPixelColor(0, strip.Color(0, 0, 0));
  strip.setPixelColor(1, strip.Color(0, 0, 0));
  strip.setPixelColor(2, strip.Color(255, 255, 255));
  strip.setPixelColor(3, strip.Color(0, 0, 0));
  strip.setPixelColor(4, strip.Color(0, 0, 0));
  strip.setPixelColor(5, strip.Color(255, 255, 255));
  strip.setPixelColor(6, strip.Color(0, 0, 0));
  strip.setPixelColor(7, strip.Color(0, 0, 0));
  strip.setPixelColor(8, strip.Color(255, 255, 255));
  strip.setPixelColor(9, strip.Color(0, 0, 0));
  strip.setPixelColor(10, strip.Color(0, 0, 0));
  strip.setPixelColor(11, strip.Color(255, 255, 255));
  strip.setPixelColor(12, strip.Color(0, 0, 0));
  strip.setPixelColor(13, strip.Color(0, 0, 0));
  strip.setPixelColor(14, strip.Color(255, 255, 255));
  strip.setPixelColor(15, strip.Color(0, 0, 0));
  strip.setPixelColor(16, strip.Color(0, 0, 0));
  strip.setPixelColor(17, strip.Color(255, 255, 255));
  strip.setPixelColor(18, strip.Color(0, 0, 0));
  strip.setPixelColor(19, strip.Color(0, 0, 0));
  strip.setPixelColor(20, strip.Color(255, 255, 255));
  strip.setPixelColor(21, strip.Color(0, 0, 0));
  strip.setPixelColor(22, strip.Color(0, 0, 0));
  strip.setPixelColor(23, strip.Color(255, 255, 255));
  strip.setPixelColor(24, strip.Color(0, 0, 0));
  strip.setPixelColor(25, strip.Color(0, 0, 0));
  strip.setPixelColor(26, strip.Color(255, 255, 255));
  strip.setPixelColor(27, strip.Color(0, 0, 0));
  strip.setPixelColor(28, strip.Color(0, 0, 0));
  strip.setPixelColor(29, strip.Color(255, 255, 255));
  strip.setPixelColor(30, strip.Color(0, 0, 0));
  strip.setPixelColor(31, strip.Color(0, 0, 0));
  strip.setPixelColor(32, strip.Color(255, 255, 255));
  strip.setPixelColor(33, strip.Color(0, 0, 0));
  strip.setPixelColor(34, strip.Color(0, 0, 0));
  strip.setPixelColor(35, strip.Color(255, 255, 255));
  strip.setPixelColor(36, strip.Color(0, 0, 0));
  strip.setPixelColor(37, strip.Color(0, 0, 0));
  strip.setPixelColor(38, strip.Color(255, 255, 255));
  strip.setPixelColor(39, strip.Color(0, 0, 0));
  strip.setPixelColor(40, strip.Color(0, 0, 0));
  strip.setPixelColor(41, strip.Color(255, 255, 255));
  strip.setPixelColor(42, strip.Color(0, 0, 0));
  strip.setPixelColor(43, strip.Color(0, 0, 0));
  strip.setPixelColor(44, strip.Color(255, 255, 255));
  strip.setPixelColor(45, strip.Color(0, 0, 0));
  strip.setPixelColor(46, strip.Color(0, 0, 0));
  strip.setPixelColor(47, strip.Color(255, 255, 255));
  strip.setPixelColor(48, strip.Color(0, 0, 0));
  strip.setPixelColor(49, strip.Color(0, 0, 0));
  strip.setPixelColor(50, strip.Color(255, 255, 255));
  strip.setPixelColor(51, strip.Color(0, 0, 0));
  strip.setPixelColor(52, strip.Color(0, 0, 0));
  strip.setPixelColor(53, strip.Color(255, 255, 255));
  strip.setPixelColor(54, strip.Color(0, 0, 0));
  strip.setPixelColor(55, strip.Color(0, 0, 0));
  strip.setPixelColor(56, strip.Color(255, 255, 255));
  strip.setPixelColor(57, strip.Color(0, 0, 0));
  strip.setPixelColor(58, strip.Color(0, 0, 0));
  strip.setPixelColor(59, strip.Color(255, 255, 255));
  strip.setPixelColor(60, strip.Color(0, 0, 0));
  strip.setPixelColor(61, strip.Color(0, 0, 0));
  strip.setPixelColor(62, strip.Color(255, 255, 255));
  strip.setPixelColor(63, strip.Color(0, 0, 0));
  strip.setPixelColor(64, strip.Color(0, 0, 0));
  strip.setPixelColor(65, strip.Color(255, 255, 255));
  strip.setPixelColor(66, strip.Color(0, 0, 0));
  strip.setPixelColor(67, strip.Color(0, 0, 0));
  strip.setPixelColor(68, strip.Color(255, 255, 255));
  strip.setPixelColor(69, strip.Color(0, 0, 0));
  strip.setPixelColor(70, strip.Color(0, 0, 0));
  strip.setPixelColor(71, strip.Color(255, 255, 255));
  strip.setPixelColor(72, strip.Color(0, 0, 0));
  strip.setPixelColor(73, strip.Color(0, 0, 0));
  strip.setPixelColor(74, strip.Color(255, 255, 255));
  strip.setPixelColor(75, strip.Color(0, 0, 0));
  strip.setPixelColor(76, strip.Color(0, 0, 0));
  strip.setPixelColor(77, strip.Color(255, 255, 255));
  strip.setPixelColor(78, strip.Color(0, 0, 0));
  strip.setPixelColor(79, strip.Color(0, 0, 0));
  strip.show();
  delay(300);
  for (int i = 0; i < 20; ++i) {
    rotate(1);
    strip.show();
    delay(300);
  }
}
void fadeBlackWhite() {
  strip.setPixelColor(0, strip.Color(0, 0, 0));
  strip.setPixelColor(1, strip.Color(0, 0, 0));
  strip.setPixelColor(2, strip.Color(0, 0, 0));
  strip.setPixelColor(3, strip.Color(0, 0, 0));
  strip.setPixelColor(4, strip.Color(0, 0, 0));
  strip.setPixelColor(5, strip.Color(0, 0, 0));
  strip.setPixelColor(6, strip.Color(0, 0, 0));
  strip.setPixelColor(7, strip.Color(0, 0, 0));
  strip.setPixelColor(8, strip.Color(0, 0, 0));
  strip.setPixelColor(9, strip.Color(0, 0, 0));
  strip.setPixelColor(10, strip.Color(0, 0, 0));
  strip.setPixelColor(11, strip.Color(0, 0, 0));
  strip.setPixelColor(12, strip.Color(0, 0, 0));
  strip.setPixelColor(13, strip.Color(0, 0, 0));
  strip.setPixelColor(14, strip.Color(0, 0, 0));
  strip.setPixelColor(15, strip.Color(0, 0, 0));
  strip.setPixelColor(16, strip.Color(0, 0, 0));
  strip.setPixelColor(17, strip.Color(0, 0, 0));
  strip.setPixelColor(18, strip.Color(0, 0, 0));
  strip.setPixelColor(19, strip.Color(0, 0, 0));
  strip.setPixelColor(20, strip.Color(0, 0, 0));
  strip.setPixelColor(21, strip.Color(0, 0, 0));
  strip.setPixelColor(22, strip.Color(0, 0, 0));
  strip.setPixelColor(23, strip.Color(0, 0, 0));
  strip.setPixelColor(24, strip.Color(0, 0, 0));
  strip.setPixelColor(25, strip.Color(0, 0, 0));
  strip.setPixelColor(26, strip.Color(0, 0, 0));
  strip.setPixelColor(27, strip.Color(0, 0, 0));
  strip.setPixelColor(28, strip.Color(0, 0, 0));
  strip.setPixelColor(29, strip.Color(0, 0, 0));
  strip.setPixelColor(30, strip.Color(0, 0, 0));
  strip.setPixelColor(31, strip.Color(0, 0, 0));
  strip.setPixelColor(32, strip.Color(0, 0, 0));
  strip.setPixelColor(33, strip.Color(0, 0, 0));
  strip.setPixelColor(34, strip.Color(0, 0, 0));
  strip.setPixelColor(35, strip.Color(0, 0, 0));
  strip.setPixelColor(36, strip.Color(0, 0, 0));
  strip.setPixelColor(37, strip.Color(0, 0, 0));
  strip.setPixelColor(38, strip.Color(0, 0, 0));
  strip.setPixelColor(39, strip.Color(0, 0, 0));
  strip.setPixelColor(40, strip.Color(0, 0, 0));
  strip.setPixelColor(41, strip.Color(0, 0, 0));
  strip.setPixelColor(42, strip.Color(0, 0, 0));
  strip.setPixelColor(43, strip.Color(0, 0, 0));
  strip.setPixelColor(44, strip.Color(0, 0, 0));
  strip.setPixelColor(45, strip.Color(0, 0, 0));
  strip.setPixelColor(46, strip.Color(0, 0, 0));
  strip.setPixelColor(47, strip.Color(0, 0, 0));
  strip.setPixelColor(48, strip.Color(0, 0, 0));
  strip.setPixelColor(49, strip.Color(0, 0, 0));
  strip.setPixelColor(50, strip.Color(0, 0, 0));
  strip.setPixelColor(51, strip.Color(0, 0, 0));
  strip.setPixelColor(52, strip.Color(0, 0, 0));
  strip.setPixelColor(53, strip.Color(0, 0, 0));
  strip.setPixelColor(54, strip.Color(0, 0, 0));
  strip.setPixelColor(55, strip.Color(0, 0, 0));
  strip.setPixelColor(56, strip.Color(0, 0, 0));
  strip.setPixelColor(57, strip.Color(0, 0, 0));
  strip.setPixelColor(58, strip.Color(0, 0, 0));
  strip.setPixelColor(59, strip.Color(0, 0, 0));
  strip.setPixelColor(60, strip.Color(0, 0, 0));
  strip.setPixelColor(61, strip.Color(0, 0, 0));
  strip.setPixelColor(62, strip.Color(0, 0, 0));
  strip.setPixelColor(63, strip.Color(0, 0, 0));
  strip.setPixelColor(64, strip.Color(0, 0, 0));
  strip.setPixelColor(65, strip.Color(0, 0, 0));
  strip.setPixelColor(66, strip.Color(0, 0, 0));
  strip.setPixelColor(67, strip.Color(0, 0, 0));
  strip.setPixelColor(68, strip.Color(0, 0, 0));
  strip.setPixelColor(69, strip.Color(0, 0, 0));
  strip.setPixelColor(70, strip.Color(0, 0, 0));
  strip.setPixelColor(71, strip.Color(0, 0, 0));
  strip.setPixelColor(72, strip.Color(0, 0, 0));
  strip.setPixelColor(73, strip.Color(0, 0, 0));
  strip.setPixelColor(74, strip.Color(0, 0, 0));
  strip.setPixelColor(75, strip.Color(0, 0, 0));
  strip.setPixelColor(76, strip.Color(0, 0, 0));
  strip.setPixelColor(77, strip.Color(0, 0, 0));
  strip.setPixelColor(78, strip.Color(0, 0, 0));
  strip.setPixelColor(79, strip.Color(0, 0, 0));
  strip.show();
  delay(100);
  for (int i = 0; i < 50; ++i) {
    brighten(5);
    strip.show();
    delay(100);
  }
}
void fadeRedGreen() {
  strip.setPixelColor(0, strip.Color(255, 0, 0));
  strip.setPixelColor(1, strip.Color(255, 0, 0));
  strip.setPixelColor(2, strip.Color(255, 0, 0));
  strip.setPixelColor(3, strip.Color(255, 0, 0));
  strip.setPixelColor(4, strip.Color(255, 0, 0));
  strip.setPixelColor(5, strip.Color(255, 0, 0));
  strip.setPixelColor(6, strip.Color(255, 0, 0));
  strip.setPixelColor(7, strip.Color(255, 0, 0));
  strip.setPixelColor(8, strip.Color(255, 0, 0));
  strip.setPixelColor(9, strip.Color(255, 0, 0));
  strip.setPixelColor(10, strip.Color(255, 0, 0));
  strip.setPixelColor(11, strip.Color(255, 0, 0));
  strip.setPixelColor(12, strip.Color(255, 0, 0));
  strip.setPixelColor(13, strip.Color(255, 0, 0));
  strip.setPixelColor(14, strip.Color(255, 0, 0));
  strip.setPixelColor(15, strip.Color(255, 0, 0));
  strip.setPixelColor(16, strip.Color(255, 0, 0));
  strip.setPixelColor(17, strip.Color(255, 0, 0));
  strip.setPixelColor(18, strip.Color(255, 0, 0));
  strip.setPixelColor(19, strip.Color(255, 0, 0));
  strip.setPixelColor(20, strip.Color(255, 0, 0));
  strip.setPixelColor(21, strip.Color(255, 0, 0));
  strip.setPixelColor(22, strip.Color(255, 0, 0));
  strip.setPixelColor(23, strip.Color(255, 0, 0));
  strip.setPixelColor(24, strip.Color(255, 0, 0));
  strip.setPixelColor(25, strip.Color(255, 0, 0));
  strip.setPixelColor(26, strip.Color(255, 0, 0));
  strip.setPixelColor(27, strip.Color(255, 0, 0));
  strip.setPixelColor(28, strip.Color(255, 0, 0));
  strip.setPixelColor(29, strip.Color(255, 0, 0));
  strip.setPixelColor(30, strip.Color(255, 0, 0));
  strip.setPixelColor(31, strip.Color(255, 0, 0));
  strip.setPixelColor(32, strip.Color(255, 0, 0));
  strip.setPixelColor(33, strip.Color(255, 0, 0));
  strip.setPixelColor(34, strip.Color(255, 0, 0));
  strip.setPixelColor(35, strip.Color(255, 0, 0));
  strip.setPixelColor(36, strip.Color(255, 0, 0));
  strip.setPixelColor(37, strip.Color(255, 0, 0));
  strip.setPixelColor(38, strip.Color(255, 0, 0));
  strip.setPixelColor(39, strip.Color(255, 0, 0));
  strip.setPixelColor(40, strip.Color(255, 0, 0));
  strip.setPixelColor(41, strip.Color(255, 0, 0));
  strip.setPixelColor(42, strip.Color(255, 0, 0));
  strip.setPixelColor(43, strip.Color(255, 0, 0));
  strip.setPixelColor(44, strip.Color(255, 0, 0));
  strip.setPixelColor(45, strip.Color(255, 0, 0));
  strip.setPixelColor(46, strip.Color(255, 0, 0));
  strip.setPixelColor(47, strip.Color(255, 0, 0));
  strip.setPixelColor(48, strip.Color(255, 0, 0));
  strip.setPixelColor(49, strip.Color(255, 0, 0));
  strip.setPixelColor(50, strip.Color(255, 0, 0));
  strip.setPixelColor(51, strip.Color(255, 0, 0));
  strip.setPixelColor(52, strip.Color(255, 0, 0));
  strip.setPixelColor(53, strip.Color(255, 0, 0));
  strip.setPixelColor(54, strip.Color(255, 0, 0));
  strip.setPixelColor(55, strip.Color(255, 0, 0));
  strip.setPixelColor(56, strip.Color(255, 0, 0));
  strip.setPixelColor(57, strip.Color(255, 0, 0));
  strip.setPixelColor(58, strip.Color(255, 0, 0));
  strip.setPixelColor(59, strip.Color(255, 0, 0));
  strip.setPixelColor(60, strip.Color(255, 0, 0));
  strip.setPixelColor(61, strip.Color(255, 0, 0));
  strip.setPixelColor(62, strip.Color(255, 0, 0));
  strip.setPixelColor(63, strip.Color(255, 0, 0));
  strip.setPixelColor(64, strip.Color(255, 0, 0));
  strip.setPixelColor(65, strip.Color(255, 0, 0));
  strip.setPixelColor(66, strip.Color(255, 0, 0));
  strip.setPixelColor(67, strip.Color(255, 0, 0));
  strip.setPixelColor(68, strip.Color(255, 0, 0));
  strip.setPixelColor(69, strip.Color(255, 0, 0));
  strip.setPixelColor(70, strip.Color(255, 0, 0));
  strip.setPixelColor(71, strip.Color(255, 0, 0));
  strip.setPixelColor(72, strip.Color(255, 0, 0));
  strip.setPixelColor(73, strip.Color(255, 0, 0));
  strip.setPixelColor(74, strip.Color(255, 0, 0));
  strip.setPixelColor(75, strip.Color(255, 0, 0));
  strip.setPixelColor(76, strip.Color(255, 0, 0));
  strip.setPixelColor(77, strip.Color(255, 0, 0));
  strip.setPixelColor(78, strip.Color(255, 0, 0));
  strip.setPixelColor(79, strip.Color(255, 0, 0));
  strip.show();
  delay(100);
  for (int i = 0; i < 50; ++i) {
    addColor(-6, 6, 0);
    strip.show();
    delay(100);
  }
}