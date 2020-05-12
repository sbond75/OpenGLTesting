#include "color.h"
#include <SDL2/SDL.h>
Color *newColor(Uint8 r, Uint8 g, Uint8 b) {
  Color *res = malloc(sizeof(Color));
  res->r = r;
  res->g = g;
  res->b = b;
  return res;
}
