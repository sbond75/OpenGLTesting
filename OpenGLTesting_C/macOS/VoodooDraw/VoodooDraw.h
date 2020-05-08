#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
#   error "Windows not yet implemented"
   //define something for Windows (32-bit and 64-bit, this part is common)
   #ifdef _WIN64
      //define something for Windows (64-bit only)
   #else
      //define something for Windows (32-bit only)
   #endif
#elif __APPLE__
    #include <TargetConditionals.h>
    #if TARGET_IPHONE_SIMULATOR
         // iOS Simulator
#       error "iOS Simulator not yet implemented"
    #elif TARGET_OS_IPHONE
        // iOS device
#       error "iOS not yet implemented"
    #elif TARGET_OS_MAC
        // Other kinds of Mac OS
        #define VOODOO_DRAW_MACOS
    #else
    #   error "Unknown Apple platform"
    #endif
#elif __linux__
#   error "Linux not yet implemented"
    // linux
#elif __unix__ // all unices not caught above
#   error "Unix not yet implemented"
    // Unix
#elif defined(_POSIX_VERSION)
#   error "POSIX not yet implemented"
    // POSIX
#else
#   error "Unknown compiler"
#endif

typedef struct VoodooDrawState {
#ifdef VOODOO_DRAW_MACOS
    void* screenSurface;
    
#endif
} VoodooDrawState;