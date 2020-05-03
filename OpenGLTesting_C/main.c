// badprog.com
#include <SDL/SDL.h>
#pragma comment(lib, "SDL2.lib")
#pragma comment(lib, "SDL2main.lib")
#pragma comment(lib, "glew32.lib")
#pragma comment(lib, "opengl32.lib")
#include <GL/glew.h>
#include <stdio.h>
#include <stdbool.h>
#include "s_new_from_stdin.h" // Because we don't have std::getline from C++...
#include <stdint.h>

#pragma region Utils
// To have getch()            //
#include <conio.h> //getch
#include <stdio.h> //puts
#ifdef _MSC_VER //visual c++ needs getch correctly defined, and _MSC_VER is defined under visual c++ only!
#define getch() _getch()
#endif
//                            //
// To have a nice system("PAUSE") for any platform! //
void pause() {
	puts("Press any key to continue...");
	getch();
}
// //
// To have nice number input  // ( http://www.cplusplus.com/forum/articles/6046/ )
int getNumberFromUser(const char* msg, int defaultIfInvalid) {
	char* input;
	int num = defaultIfInvalid;
	char* p;
	size_t len;

	puts(msg);

	input = s_new_from_stdin(false, &len, NULL);
	if (input) {
		long converted = strtol(input, &p, 10);
		if (*p) {
			// conversion failed because the input wasn't a number
			printf("Invalid input; using %d!\n", defaultIfInvalid);
		}
		else if (len == 0) { //then the user just pressed enter and that was it
			printf("Using %d!\n", defaultIfInvalid);
		}
		else {
			// use converted
			num = (int)converted;
		}

		free(input);
	}
	else {
		puts( "<NULL>" );
	}

	return num;
}

//Prints out an error message and exits the game
void fatalError(const char* errorString) {
	//std::cout << errorString << std::endl;
	//std::cout << "Enter any key to quit...";
	//int tmp;
	//std::cin >> tmp;
	//SDL_Quit();
	//exit(69);
}
#pragma endregion

float vertices[] = {
	-0.5f, -0.5f, 0.0f,
	0.5f, -0.5f, 0.0f,
	0.0f,  0.5f, 0.0f
};

const GLchar* vertexShaderSource = "#version 330 core\n\
layout (location = 0) in vec3 aPos;\n\
void main()\n\
{\n\
	gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n\
}";

const GLchar* fragmentShaderSource = "#version 330 core\n\
out vec4 FragColor;\n\
void main()\n\
{\n\
	FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n\
}";

// Returns the shader that was created.
GLuint loadShader(GLenum shaderType, const GLchar* shaderSource) {
	// Create a shader and store it in `shaderID`.
	GLuint shaderID; // The shader ID.
	shaderID = glCreateShader(shaderType);

	// Attach the source code to the shader.
	// Arguments: the shader ID, the number of source code strings, the array (or one) source code string, array of string lengths.
	glShaderSource(shaderID, 1, &shaderSource, NULL);
	// Compile the shader.
	glCompileShader(shaderID);

	// Check for errors: //

	// The simple way:
	/* GLint success;
	char  infoLog[512];
	glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);

	if(!success)
	{
		glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
		std::cout << "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n" << infoLog << std::endl;
	} */
	
	// The better way:
	GLint success = 0;
	glGetShaderiv(shaderID, GL_COMPILE_STATUS, &success);

	if (success == GL_FALSE)
	{
		GLint maxLength;
		glGetShaderiv(shaderID, GL_INFO_LOG_LENGTH, &maxLength);

		GLchar* errorLog = alloca(sizeof(GLchar) * maxLength);
		glGetShaderInfoLog(shaderID, maxLength, &maxLength, errorLog);

		glDeleteShader(shaderID);

		printf("%s\n", errorLog);
		fatalError("Shader failed to compile");
		return 0; // 0 indicates no shader.
	}

	return shaderID;
}

// Creates a shader program for the given shaders, and always cleans up (deletes) the given shaders,
// including if an error is encountered.
GLuint createAndLinkProgram(GLuint vertexShaderID, GLuint fragmentShaderID) {
	GLuint shaderProgramID;
	shaderProgramID = glCreateProgram();

	// Attach both shaders to the shaderProgramID.
	glAttachShader(shaderProgramID, vertexShaderID);
	glAttachShader(shaderProgramID, fragmentShaderID);
	glLinkProgram(shaderProgramID);

	// Check for errors in linking.
	GLint isLinked = 0;
	glGetProgramiv(shaderProgramID, GL_LINK_STATUS, &isLinked);
	if (isLinked == GL_FALSE)
	{
		GLint maxLength;
		glGetShaderiv(shaderProgramID, GL_INFO_LOG_LENGTH, &maxLength);

		GLchar* errorLog = alloca(sizeof(GLchar) * maxLength);
		glGetShaderInfoLog(shaderProgramID, maxLength, &maxLength, errorLog);

		glDeleteShader(shaderProgramID);

		glDeleteProgram(shaderProgramID);
		glDeleteShader(vertexShaderID);
		glDeleteShader(fragmentShaderID);

		printf("%s\n", errorLog);
		fatalError("Shaders failed to link!");
		return 0;
	}
	
	// "Oh yeah, and don't forget to delete the shader objects once we've
	// linked them into the program object; we no longer need them anymore:"
	glDeleteShader(vertexShaderID);
	glDeleteShader(fragmentShaderID);  

	return shaderProgramID;
}

void thing()
{
	GLuint VBO;
	// Generate 1 VBO (arg 1); put its ID into the array
	// (an array of one element is ok) given (arg 2).
	glGenBuffers(1, &VBO);

	// Make the buffer bound into GL_ARRAY_BUFFER as the current active buffer
	// for GL_ARRAY_BUFFER; VBO is the buffer to use to bind.
	glBindBuffer(GL_ARRAY_BUFFER, VBO);

	//printf("%d", sizeof(vertices)); // Evaluates to 36 because 9 floats, each 4 bytes in size.
	//pause();

	// Uploads the data to the GPU.
	// Arguments: target buffer; amount of data; pointer to the data; usage of data.
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

	GLuint vertexShaderID = loadShader(GL_VERTEX_SHADER, vertexShaderSource);
	if (!vertexShaderID) {
		__debugbreak(); // Error
	}
	GLuint fragmentShaderID = loadShader(GL_FRAGMENT_SHADER, fragmentShaderSource);
	if (!fragmentShaderID) {
		__debugbreak(); // Error
	}

	GLuint programID = createAndLinkProgram(vertexShaderID, fragmentShaderID);
	if (!programID) {
		__debugbreak(); // Error
	}

	// Bind this program as active.
	glUseProgram(programID);

	pause();

	// //
}

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

	thing();
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
