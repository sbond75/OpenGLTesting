{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
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
import Foreign.C.Types
import Foreign.C.String
import Data.Bits
import Data.IORef
import Control.Monad
import Control.Monad.Loops
import GHC.Ptr
import SDL.Event

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

displayMe :: MonadIO m => m ()
displayMe = do glClear GL_COLOR_BUFFER_BIT
               drawRect (0.0,0.0) (0.5,0.5)

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

appLoop :: MonadIO m => m ()
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
                    
