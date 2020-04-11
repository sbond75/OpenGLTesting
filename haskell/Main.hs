{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-
OpenGL library -- commands (function points to dynamic library),
"glew" loads the drivers
Driver -- examples:

- NVidia, AMD, Intel, Microsoft software rendering for opengl 1.1
OS - set up a window, how to init OpenGL in the window
(create context),
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
-- import Graphics.GLU



displayMe = do glClear GL_COLOR_BUFFER_BIT
               glBegin GL_POLYGON
               glVertex3f 0.0 0.0 0.0
               glVertex3f 0.5 0.0 0.0
               glVertex3f 0.5 0.5 0.0
               glVertex3f 0.0 0.5 0.0
               glEnd
               glFlush

window s = createWindow s
                        SDL_WINDOWPOS_UNDEFINED
                        SDL_WINDOWPOS_UNDEFINED
                        640
                        480
                        (SDL_WINDOW_SHOWN .|. SDL_WINDOW_OPENGL)

eprintf s = putStr s *> exitFailure

eventLoop :: IO Bool
eventLoop = do e <- pollEvent
               case e of
                 Nothing -> return False
                 Just p -> case eventPayload p of
                             QuitEvent -> return True
                             _ -> return False
appLoop = do delay 15
             glClear GL_COLOR_BUFFER_BIT
             displayMe

main = do c <- init SDL_INIT_VIDEO
          when (c < 0) exitFailure
          w <- withCString "SDL_Tutorial" window
          when (w == nullPtr)
               (do s <- join (peekCString <$> getError)
                   putStrLn s
                   eprintf "Window could not be created!\n")
          context <- glCreateContext w
          when (context == nullPtr)
               (eprintf "OpenGL context could be created!\n")
          glClearColor 0 1 0 1
          whileM_ (not <$> eventLoop) (appLoop *> glSwapWindow w)
          glDeleteContext context
          destroyWindow w
          quit
                    
