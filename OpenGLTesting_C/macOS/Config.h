//
//  Config.h
//  OpenGLTesting
//
//  Created by sbond75 on 5/10/20.
//  Copyright © 2020 sbond75. All rights reserved.
//

#ifndef Config_h
#define Config_h

// Configurable setting
#define USE_CUSTOM_RENDERER_EEPIXELVIEWER

// Configurable setting
#define USE_HASKELL_EXPORTS
#ifdef USE_HASKELL_EXPORTS
#include "Picture_stub.h"
#include <HsFFI.h>
#endif

// Configurable setting
// #define USE_ALPHA

// Screen dimension constants
extern const int SCREEN_WIDTH;
extern const int SCREEN_HEIGHT;

#endif /* Config_h */
