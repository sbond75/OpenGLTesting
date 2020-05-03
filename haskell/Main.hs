{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}
{-
OpenGL library -- commands (function points to dynamic library),
"glew" loads the drivers
Driver -- examples:

- NVidia, AMD, Intel, Microsoft software rendering for opengl 1.1
OS - set up a window, how to init OpenGL in the window
(create context),

Buffer - location of memory to put data
Color buffers - a buffer of RGB

Immediate mode - commands are issued between glBegin and glEnd
-}

module Main where

import Graphics.GL
import Control.Monad.IO.Class
import SDL.Raw hiding (pollEvent, QuitEvent)
import Prelude hiding (init)
import System.Exit
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Bits
import Data.IORef
import Control.Monad
import Control.Monad.Loops
import GHC.Ptr
import SDL.Event
import Foreign.Marshal
import Foreign.Marshal.Array

-- Run glBegin with a GL enum, a body consisting of immediate mode
-- commands, and wrap it between calls to glBegin and glEnd
withGLMode :: MonadIO m => GLenum -> m a -> m ()
withGLMode mode body = do glBegin mode
                          body
                          glEnd
                          glFlush

type Coord = (Float, Float)

drawRect :: MonadIO m => Coord -> Coord -> m ()
drawRect (x1, y1) (x2, y2) = withGLMode GL_POLYGON
                                        (do glVertex3f x1 y1 0.0
                                            glVertex3f x2 y1 0.0
                                            glVertex3f x2 y2 0.0
                                            glVertex3f x1 y2 0.0)

displayMe :: IO ()
displayMe = do glClear GL_COLOR_BUFFER_BIT
               thing
               -- drawRect (0.0,0.0) (0.5,0.5)
               

window :: MonadIO m => CString -> m Window
window s = createWindow s
                        SDL_WINDOWPOS_UNDEFINED
                        SDL_WINDOWPOS_UNDEFINED
                        640
                        480
                        (SDL_WINDOW_SHOWN .|. SDL_WINDOW_OPENGL)

eprintf :: String -> IO a
eprintf s = putStr s *> exitFailure

eventLoop :: MonadIO m => m Bool
eventLoop = do e <- pollEvent
               case e of
                 Nothing -> return False
                 Just p -> case eventPayload p of
                             QuitEvent -> return True
                             _ -> return False

shaderSource = "#version 300 core\n\
\layout (location = 0) in vec3 aPos;\n\
\void main()\n\
\{\n\
\    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n\
\}"

allocaPeek inner = alloca $ \ptr -> do inner ptr; peek ptr
-- thing :: MonadIO m => m ()
thing = alloca $
         \(vbo :: Ptr GLuint) -> do
           putStrLn "vbo"
           withArray vertices $
              \vs -> do -- vs is a pointer to vertices
                putStrLn "vs"
                x <- peek vs
                print x
                print (sizeOf x)
                print vbo
                glGenBuffers 1 vbo
                vbo <- peek vbo
                glBindBuffer GL_ARRAY_BUFFER vbo
                glBufferData GL_ARRAY_BUFFER 36 vs GL_STATIC_DRAW
                vertexShader <- glCreateShader GL_VERTEX_SHADER
                putStrLn "shader created"
                withCString shaderSource $
                  \s -> do
                    putStrLn "s"
                    alloca $
                     \vss -> do -- vertex shader sources
                       putStrLn "vss"
                       v <- newCString shaderSource
                       poke vss v
                       glShaderSource vertexShader 1 vss nullPtr
                       glCompileShader vertexShader
                       alloca $
                         \success -> do
                           glGetShaderiv vertexShader GL_COMPILE_STATUS success
                           s <- peek success
                           when (s /= 0)
                                (alloca $
                                  \maxLength -> do
                                    glGetShaderiv vertexShader GL_INFO_LOG_LENGTH maxLength
                                    len <- peek maxLength
                                    allocaArray (fromIntegral len) $
                                      \errorLog -> do
                                        glGetShaderInfoLog vertexShader len maxLength errorLog
                                        glDeleteShader vertexShader
                                        s <- peekCString errorLog
                                        putStrLn s
                                        eprintf "Shader failed to initalize")
                exitSuccess

  where
    vertices :: [Float]
    vertices = [-0.5, -0.5, 0.0,
                0.5, -0.5, 0.0,
                0.0, 0.5, 0.0]
  

-- appLoop :: MonadIO m => m ()
appLoop = do delay 15
             glClear GL_COLOR_BUFFER_BIT
             displayMe

main :: IO ()
main = do c <- init SDL_INIT_VIDEO
          when (c < 0) exitFailure
          w <- withCString "SDL Tutorial" window
          when (w == nullPtr)
               (do p <- getError
                   s <- peekCString p
                   putStrLn s
                   eprintf "Window could not be created!\n")
          context <- glCreateContext w
          when (context == nullPtr)
               (eprintf "OpenGL context could be created!\n")
          -- Sets a global RBGA value, the clear color
          -- This is green
          glClearColor 0 1 0 1
          whileM_ (not <$> eventLoop) (appLoop *> glSwapWindow w)
          glDeleteContext context
          destroyWindow w
          quit
                    
