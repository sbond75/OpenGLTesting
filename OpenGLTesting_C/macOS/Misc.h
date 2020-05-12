//
//  Misc.h
//  OpenGLTesting
//
//  Created by sbond75 on 5/10/20.
//  Copyright © 2020 sbond75. All rights reserved.
//

#ifndef Misc_h
#define Misc_h

#include <stdbool.h>
#include <stdio.h>
#include "Timing.h"
#include <stdint.h>

typedef struct Color {
  Uint8 r, g, b
#ifdef USE_ALPHA
      ,
      a
#endif
      ;
} Color;

static inline Uint8 calcA(int x, int y, int t) { return 0; }
#ifndef USE_HASKELL_EXPORTS
Uint8 calcR_(x, y, t) { return x + 20; }
Uint8 calcG_(x, y, t) { return y + 40; }
Uint8 calcB_(x, y, t) { return y + 1; }
#define calcR calcR_
#define calcG calcG_
#define calcB calcB_
#endif

#endif /* Misc_h */
