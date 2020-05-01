// badprog.com
#include <SDL/SDL.h>
#pragma comment(lib, "SDL2.lib")
#pragma comment(lib, "SDL2main.lib")
#pragma comment(lib, "glew32.lib")
#pragma comment(lib, "opengl32.lib")
#include <GL/glew.h>
#include <stdio.h>
#include <stdbool.h>

void drawRect(float x1, float y1, float x2, float y2)
{
	glBegin(GL_POLYGON);
		glVertex3f(x1, y1, 0.0f);
		glVertex3f(x2, y1, 0.0f);
		glVertex3f(x2, y2, 0.0f);
		glVertex3f(x1, y2, 0.0f);
	glEnd();
}

void displayMe(void)
{
    glClear(GL_COLOR_BUFFER_BIT);

	// Set the drawing color to white.
	glColor3f(1.0f, 1.0f, 1.0f);

	drawRect(0.0f, 0.0f, 0.5f, 0.5f);

	// Set the drawing color to blue (r,g,b <-- 1).
	glColor3f(0.0f, 0.0f, 1.0f);

	drawRect(-0.2f, -0.2f, -0.4f, -0.3f);

	// Draw a cool triangle with interpolated colors.
	glBegin(GL_TRIANGLES);
	glColor3f(0.8f, 0.1f, 0.0f);
	glVertex2f(0, 0);
	glColor3f(0.0f, 0.5f, 0.2f);
	glVertex2f(1, 0);
	glColor3f(0.8f, 0.8f, 0.0);
	glVertex2f(0, 1);
	glEnd();

    glFlush();
}

//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

int main(int argc, char** argv)
{
	// The window we'll be rendering to
	SDL_Window* window = NULL;

	// OpenGL context
	SDL_GLContext context = NULL;

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
	window = SDL_CreateWindow("SDL Tutorial", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN | SDL_WINDOW_OPENGL);
	if (window == NULL) {
		fprintf(stderr, "Window could not be created! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Create context
	context = SDL_GL_CreateContext(window);
	if (context == NULL) {
		fprintf(stderr, "OpenGL context could not be created! SDL_Error: %s\n", SDL_GetError());
		return 1;
	}

	// Set up glew (optional but recommended)
	GLenum error = glewInit();
	if (error != GLEW_OK) {
		/* Problem: glewInit failed, something is seriously wrong. */
		fprintf(stderr, "Could not initialize glew! Error: %s\n", glewGetErrorString(error));
	}
	// TODO: deinit glew?

	fprintf(stdout, "Status: Using GLEW %s\n", glewGetString(GLEW_VERSION));
	// Check the OpenGL version
	printf("***   OpenGL Version: %s   ***\n", glGetString(GL_VERSION));

	// Set the global clear color.
	glClearColor(0.0f, 1.0f, 0.0f, 1.0f);

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

		// Render
		displayMe();

		SDL_GL_SwapWindow(window);

		// Delay
		SDL_Delay(15);
	}

	/* Delete our opengl context, destroy our window, and shutdown SDL */
	SDL_GL_DeleteContext(context);
	SDL_DestroyWindow(window);
	SDL_Quit();
    return 0;
}
