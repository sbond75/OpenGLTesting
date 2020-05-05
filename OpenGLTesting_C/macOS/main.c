#include <HsFFI.h>
#include "Picture_stub.h"


#include <SDL2/SDL.h>
// #pragma comment(lib, "SDL2.lib")
// #pragma comment(lib, "SDL2main.lib")
#include <stdio.h>
#include <stdbool.h>
#include "s_new_from_stdin.h" // Because we don't have std::getline from C++...
#include <stdint.h>

#define USE_ALPHA

//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

typedef struct Color {
	Uint8 r, g, b
#ifdef USE_ALPHA
	,a
#endif
	;
} Color;

Color funcFromPointToColor(int x, int y) {
  return (Color) { .r = calcR(x,y), .g = calcG(x,y), .b = calcB(x,y)
#ifdef USE_ALPHA
	, .a = calcA(x,y)
#endif
   };
}

void drawPixel(int x, int y, SDL_Renderer* renderer, Color c) {
#ifdef USE_ALPHA
	const Uint8 alpha = c.a;
#else
	const Uint8 alpha = SDL_ALPHA_OPAQUE;
#endif
	SDL_SetRenderDrawColor(renderer, c.r, c.g, c.b, alpha);
	SDL_RenderDrawPoint(renderer, x, y);
}

int main(int argc, char** argv)
{
        hs_init(&argc, &argv);
	// The window we'll be rendering to
	SDL_Window* window = NULL;

	// Renderer
	SDL_Renderer* renderer = NULL;

	// Main loop flag
	bool quit = false;

	// Event handler
	SDL_Event e;

	// Initialize SDL
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		fprintf(stderr, "SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Create window
	// "SDL Tutorial"
	if (SDL_CreateWindowAndRenderer(SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN, &window, &renderer)) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Couldn't create window and renderer: %s", SDL_GetError());
		return 3;
	}
	if (window == NULL) {
		fprintf(stderr, "Window could not be created! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

#ifdef USE_ALPHA
	// Disable alpha blending for performance.
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
#endif

	// Render the pixels:
	for (int i = 0; i < SCREEN_WIDTH; ++i) {
		for (int j = 0; j < SCREEN_HEIGHT; ++j) {
			Color c = funcFromPointToColor(i, j);
			drawPixel(i, j, renderer, c);
		}
	}

	// Present our pixels
	SDL_RenderPresent(renderer);

	// While application is running
	while (!quit) {
		// Handle events on queue
		while (SDL_PollEvent(&e) != 0)
		{
			// User requests quit
			if (e.type == SDL_QUIT)
			{
				quit = true;
			}
		}

		// Delay
		SDL_Delay(15);
	}

	/* Destroy our renderer, destroy our window, and shutdown SDL */
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();
    hs_exit();
    return 0;
}
