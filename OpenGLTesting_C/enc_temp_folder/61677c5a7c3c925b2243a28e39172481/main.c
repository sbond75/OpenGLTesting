#include <SDL/SDL.h>
#pragma comment(lib, "SDL2.lib")
#pragma comment(lib, "SDL2main.lib")
#include <stdio.h>
#include <stdbool.h>
#include "s_new_from_stdin.h" // Because we don't have std::getline from C++...
#include <stdint.h>

//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

typedef struct Color {
	Uint8 r, g, b;
} Color;

Color funcFromPointToColor(int x, int y) {
	return (Color) { .r = x + 10, .g = y + 23, .b = x + 2 };
}

void drawPixel(int x, int y, SDL_Renderer* renderer, Uint8 r, Uint8 g, Uint8 b) {
	SDL_SetRenderDrawColor(renderer, r, g, b, SDL_ALPHA_OPAQUE);
	SDL_RenderDrawPoint(renderer, x, y);
}

int main(int argc, char** argv)
{
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

	// Disable alpha blending for performance.
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);

	// Render the pixels:
	for (int i = 0; i < SCREEN_WIDTH; ++i) {
		for (int j = 0; j < SCREEN_HEIGHT; ++j) {
			Color c = funcFromPointToColor(i, j);
			drawPixel(i, j, renderer, c.r, c.g, c.b);
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
    return 0;
}