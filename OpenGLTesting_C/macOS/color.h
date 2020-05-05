#include <SDL2/SDL.h>
typedef struct Color {
	Uint8 r, g, b;
} Color;

extern Color *newColor(Uint8 r, Uint8 g, Uint8 b);
