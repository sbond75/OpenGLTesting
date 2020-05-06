#include <HsFFI.h>
//#include "Picture_stub.h"


#include <SDL2/SDL.h>
// #pragma comment(lib, "SDL2.lib")
// #pragma comment(lib, "SDL2main.lib")
#include <stdio.h>
#include <stdbool.h>
#include "s_new_from_stdin.h" // Because we don't have std::getline from C++...
#include <stdint.h>

//#define USE_ALPHA

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

Uint8 calcR(x,y,t) {
	return x+20;
}
Uint8 calcG(x,y,t) {
	return y+40;
}
Uint8 calcB(x,y,t) {
	return y+1;
}

// Takes x and y positions, and time.
Color funcFromPointToColor(int x, int y, int t) {
  return (Color) { .r = calcR(x+t,y+t,t), .g = calcG(x+t,y+t,t), .b = calcB(x+t,y+t,t)
#ifdef USE_ALPHA
	, .a = calcA(x+t,y+t,t)
#endif
   };
}

void drawPixel(size_t i, Uint8* pixels, SDL_PixelFormat* format, Color c) {
	pixels[i * sizeof(Color)] = SDL_MapRGB(format, c.r, c.g, c.b);

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
int render(SDL_Renderer* renderer, SDL_Texture* screen, SDL_PixelFormat* format, int t) {
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
		size_t j = (i / SCREEN_WIDTH) % SCREEN_HEIGHT;
		Color c = funcFromPointToColor(i, j, t);
		drawPixel(i, pixels, format, c);
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
	printf("Elapsed milliseconds: %d\n", elapsed);

	return 0;
}

int main(int argc, char** argv)
{
	// Initialize the Haskell runtime system.
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
	window = SDL_CreateWindow(NULL /*"SDL Tutorial"*/,
							  SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 
						 	  SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN); 
	if (window == NULL) {
		fprintf(stderr, "Window could not be created! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Create renderer
	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE /*SDL_RENDERER_ACCELERATED*/);
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
	SDL_PixelFormat* format = SDL_AllocFormat(form);
	if (format == NULL) {
		fprintf(stderr, "SDL_AllocFormat() failed! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

#ifndef USE_ALPHA
	// Disable alpha blending for performance.
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
#endif

	int t = 0;
	render(renderer, screen, format, t);

	// While application is running
	while (!quit) {
		// Handle events on queue
		while (SDL_PollEvent(&e) != 0)
		{
			// User requests quit
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
			case SDL_QUIT:
				quit = true;
				break;
			}
		}

		// Delay
		//SDL_Delay(15);

		// Render
		t++;
		render(renderer, screen, format, t);
	}

	/* Destroy our renderer, destroy our window, and shutdown SDL */
	SDL_DestroyTexture(screen);
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();
    hs_exit();
    return 0;
}
