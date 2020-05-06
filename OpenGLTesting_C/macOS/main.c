//#define USE_HASKELL_EXPORTS // Configurable setting
#ifdef USE_HASKELL_EXPORTS
#include <HsFFI.h>
#include "Picture_stub.h"
#endif

#include <SDL2/SDL.h>
// #pragma comment(lib, "SDL2.lib")
// #pragma comment(lib, "SDL2main.lib")
#include <stdio.h>
#include <stdbool.h>
//#include "s_new_from_stdin.h" // Because we don't have std::getline from C++...
#include <stdint.h>
#include "Timing.h"

//#define USE_ALPHA // Configurable setting

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

Uint8 calcA(int x, int y, int t) { return 0; }
#ifndef USE_HASKELL_EXPORTS
Uint8 calcR_(x,y,t) {
	return x+20;
}
Uint8 calcG_(x,y,t) {
	return y+40;
}
Uint8 calcB_(x,y,t) {
	return y+1;
}
#define calcR calcR_
#define calcG calcG_
#define calcB calcB_
#endif
// Takes x and y positions, and time.
static inline Color funcFromPointToColor(int x, int y, int t) {
  return (Color) { .r = calcR(x+t,y+t), .g = calcG(x+t,y+t), .b = calcB(x+t,y+t)
#ifdef USE_ALPHA
	, .a = calcA(x+t,y+t,t)
#endif
   };
}

static inline void drawPixel(size_t i, Uint8* pixels, Color c) {
	pixels[i*3] = c.r;
	pixels[i*3 + 1] = c.g;
	pixels[i*3 + 2] = c.b;

	/*
	pixels[offset]     = c.r; // r, _, _, r, _, _, ...
	pixels[offset + 1] = c.g;
	pixels[offset + 2] = c.b;
#ifdef USE_ALPHA
	pixels[offset + 3] = c.a;
#endif
	*/
}

// t is time.
int render(SDL_Renderer* renderer, SDL_Texture* screen, int t) {
	Uint32 start = SDL_GetTicks();
	// This line is only needed if we want to render over time,
	// pixel by pixel and present between pixels.
	//SDL_RenderClear(renderer);

	// Lock the texture for rendering so that we can write pixels to it.
	int pitch = SCREEN_WIDTH * sizeof(Color); // Width (in bytes) of a single row in the texture.
	Uint8* pixels = NULL; // r, g, b, r, g, b, ...
	if (SDL_LockTexture(screen,
                		NULL,      // NULL means the *whole texture* here.
                		(void**)&pixels,
                		&pitch)) { // After SDL_LockTexture, `pixels` can be used for writing.
		fprintf(stderr, "Error locking texture! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Fill the `pixels` array:
	for (size_t i = 0; i < SCREEN_WIDTH * SCREEN_HEIGHT; ++i) {
		size_t x = i % SCREEN_WIDTH;
		size_t y = i / SCREEN_WIDTH;
		Color c = funcFromPointToColor(x, y, t);
		drawPixel(i, pixels, c);
	}
	/*
	SDL_SetRenderTarget(renderer, NULL);
	SDL_RenderCopy(renderer, screen, NULL, NULL);
	SDL_RenderPresent(renderer);
	SDL_SetRenderTarget(renderer, screen);
	*/

	// Clean up with SDL_UnlockTexture:
	SDL_UnlockTexture(screen);
	
	// Then you can RenderCopy this texture as normal:
	if (SDL_RenderCopy(renderer, screen, NULL, NULL)) {
		fprintf(stderr, "Error in SDL_RenderCopy()! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Present our pixels
	SDL_RenderPresent(renderer);

	Uint32 end = SDL_GetTicks();
	Uint32 elapsed = end - start;
	printf("render(): Elapsed milliseconds: %d\n", elapsed);

	return 0;
}

int main(int argc, char** argv)
{
#ifdef USE_HASKELL_EXPORTS
	// Initialize the Haskell runtime system.
    hs_init(&argc, &argv);
#endif
	
	// The window we'll be rendering to
	SDL_Window* window = NULL;

	// Renderer
	SDL_Renderer* renderer = NULL;

	// Main loop flag
	bool quit = false;

	// Event handler
	SDL_Event e;

	// FpsLimiter
	FpsLimiter fpsLimiter;
	FpsLimiter_init(&fpsLimiter, 60.0f); // Set the FPS.

	// Initialize SDL
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		fprintf(stderr, "SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Create window
	window = SDL_CreateWindow(NULL /*"SDL Tutorial"*/,
							  SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 
						 	  SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN); 
	if (window == NULL) {
		fprintf(stderr, "Window could not be created! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Create renderer
	//Uint32 rendererFlags = SDL_RENDERER_SOFTWARE;
	Uint32 rendererFlags = SDL_RENDERER_ACCELERATED;
	renderer = SDL_CreateRenderer(window, -1, rendererFlags);
	if (renderer == NULL) {
		fprintf(stderr, "Renderer could not be created! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Create the texture.
#ifdef USE_ALPHA
	Uint32 form = SDL_PIXELFORMAT_RGBA8888;
#else
	Uint32 form = SDL_PIXELFORMAT_RGB24;
#endif
	SDL_Texture* screen = SDL_CreateTexture(renderer, form, SDL_TEXTUREACCESS_STREAMING, SCREEN_WIDTH, SCREEN_HEIGHT);
	if (screen == NULL) {
		fprintf(stderr, "SDL_CreateTexture() failed! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

#ifndef USE_ALPHA
	// Disable alpha blending for performance.
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
#endif

	int t = 0;

	// While application is running
	while (!quit) {
		// Indicate the beginning of a "frame".
		FpsLimiter_begin(&fpsLimiter);

		// Handle events on queue
		while (SDL_PollEvent(&e) != 0)
		{
			Sint32 k;
			Uint32 ticksBefore, ticksAfter;
			switch (e.type) {
			case SDL_KEYDOWN:
				k = e.key.keysym.sym;
				if (k == SDLK_r) {
					// Redraw
					ticksBefore = SDL_GetTicks();
					SDL_RenderPresent(renderer);
					ticksAfter = SDL_GetTicks();
					printf("Time elapsed for redraw: %d\n", ticksAfter - ticksBefore);
					fflush(stdout);
				}
				break;
			case SDL_KEYUP:
				//Sint32 k = e.key.keysym.sym;
				break;
			// User requests quit
			case SDL_QUIT:
				quit = true;
				break;
			}
		}
		if (quit) break;

		// Delay
		//SDL_Delay(15);

		// Render
		puts("--");
		render(renderer, screen, t);
		t++;

		// Delay automatically if needed to limit to the configured framerate.
		float fps = FpsLimiter_end(&fpsLimiter);

		printf("FPS: %.1f\n", fps);
		printf("FrameTime: %.1f\n", fpsLimiter._frameTime);
	}

	/* Destroy our renderer, destroy our window, and shutdown SDL */
	SDL_DestroyTexture(screen);
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();

#ifdef USE_HASKELL_EXPORTS
	// De-initialize the Haskell runtime system.
    hs_exit();
#endif
    return 0;
}
