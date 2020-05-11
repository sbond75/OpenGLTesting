#include <SDL2/SDL.h>
#include <SDL2/SDL_syswm.h>
#import <Cocoa/Cocoa.h>
#include "VoodooDraw.h"
#import "Impl_EEPixelViewerGitHub.h"

// Returns true if success, false if not.
// Fills the given struct.
bool getWMInfo(SDL_Window* window, SDL_SysWMinfo* wmInfo) {
#define info (*wmInfo)
  // https://wiki.libsdl.org/SDL_GetWindowWMInfo

  SDL_VERSION(&(info).version); /* initialize info structure with SDL version info */

  if(SDL_GetWindowWMInfo(window,&(info))) { /* the call returns true on success */
    /* success */
    const char *subsystem = "an unknown system!";
    switch((info).subsystem) {
      case SDL_SYSWM_UNKNOWN:   break;
      case SDL_SYSWM_WINDOWS:   subsystem = "Microsoft Windows(TM)";  break;
      case SDL_SYSWM_X11:       subsystem = "X Window System";        break;
#if SDL_VERSION_ATLEAST(2, 0, 3)
      case SDL_SYSWM_WINRT:     subsystem = "WinRT";                  break;
#endif
      case SDL_SYSWM_DIRECTFB:  subsystem = "DirectFB";               break;
      case SDL_SYSWM_COCOA:     subsystem = "Apple OS X";             break;
      case SDL_SYSWM_UIKIT:     subsystem = "UIKit";                  break;
#if SDL_VERSION_ATLEAST(2, 0, 2)
      case SDL_SYSWM_WAYLAND:   subsystem = "Wayland";                break;
      case SDL_SYSWM_MIR:       subsystem = "Mir";                    break;
#endif
#if SDL_VERSION_ATLEAST(2, 0, 4)
      case SDL_SYSWM_ANDROID:   subsystem = "Android";                break;
#endif
#if SDL_VERSION_ATLEAST(2, 0, 5)
      case SDL_SYSWM_VIVANTE:   subsystem = "Vivante";                break;
#endif
    }

    SDL_Log("This program is running SDL version %d.%d.%d on %s",
        (int)((info).version.major),
        (int)((info).version.minor),
        (int)((info).version.patch),
        subsystem);
    return true;
  } else {
    /* call failed */
    SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Couldn't get window information: %s", SDL_GetError());
    return false;
  }
#undef info
}

#if 0
// https://github.com/spurious/SDL-mirror/blob/master/src/video/cocoa/SDL_cocoawindow.m
/* @interface SDLView : NSView {
    SDL_Window *_sdlWindow;
} */

/* void createBuffer(size_t width, size_t height) {
#define BYTES_PER_PIXEL 3
    vImageBuffer buf;
    buf.data = malloc(BYTES_PER_PIXEL * width * height);
    buf.height = height;
    buf.width = width;
    buf.rowBytes = 
} */

CoreSurfaceBufferRef createBuffer(int width, int height) {
    CFMutableDictionaryRef dict;
    int x = width, y = height, pitch = x * 2, size = 2 * x * y, i;
    char *pixelFormat = "565L"; // Each pixel is 16 bits for optimization. 5 bits for red, 6 for green, 5 for blue.

    /* Create a screen surface */
    dict = CFDictionaryCreateMutable(kCFAllocatorDefault, 0,
        &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    CFDictionarySetValue(dict, kCoreSurfaceBufferGlobal, kCFBooleanTrue);
    CFDictionarySetValue(dict, kCoreSurfaceBufferPitch,
        CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &pitch));
    CFDictionarySetValue(dict, kCoreSurfaceBufferWidth,
        CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &x));
    CFDictionarySetValue(dict, kCoreSurfaceBufferHeight,
        CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &y));
    CFDictionarySetValue(dict, kCoreSurfaceBufferPixelFormat,
        CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type,
        pixelFormat));
    CFDictionarySetValue(dict, kCoreSurfaceBufferAllocSize,
        CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &size));

    CoreSurfaceBufferRef screenSurface = CoreSurfaceBufferCreate(dict);
    return screenSurface;
}

@interface CustomView : NSObject {
    // Instance variables
    CADisplayLink* displayLink;
}
@implementation CustomView
@implementation MainView
// "To convert RGB values to 16-bit values suitable for writing to such a buffer, you can define a macro, allowing your application to move smoothly between 16-bit and 32-bit RGB:"
// Converts the given bytes (R, G, B) into a single 16-bit value.
#define RGB2565L(R, G, B) ((R >> 3) << 11) | (( G >> 2) << 5 ) \
    | (( B >> 3 ) << 0 )

- (id)initWithFrame:(CGRect)rect {

    self = [ super initWithFrame: rect ];
    if (nil != self) {

    return self;
}
-(void)createBuffer:  {
}
-(void)setupDisplayLink {
    self.link = [CADisplayLink displayLinkWithTarget: self, selector: @selector(drawStuff)];
    [link addToRunLoop: [NSRunLoop mainRunLoop], NSDefaultRunLoopMode];
}
-(void)drawStuff {
    CoreSurfaceBufferRef screenSurface;
    Uint16* baseAddress = CoreSurfaceBufferGetBaseAddress(screenSurface);


    self.layer.setNeedsDisplay();
}

-(void)drawLayer:(CALayer *)layer inContext:(CGContextRef)ctx {
    CALayer drawingLayer = self.drawingLayer ?? CAShapeLayer()
}
@end

// Returns initialized state if success, else NULL.
VoodooDrawState test(SDL_Window* window) {
    VoodooDrawState s;

    SDL_SysWMinfo info;
    if (!getSDLInfo(window, info)) {
        return NULL;
    }
    
    @autoreleasepool {
        NSWindow* window = info.cocoa.window;
        NSView* view = /*(SDLView*)*/window.contentView; // https://developer.apple.com/documentation/appkit/nswindow/1419160-contentview?language=objc
        
        CoreSurfaceBufferRef screenSurface = createBuffer(view.rect.size.width, view.rect.size.height);
        CoreSurfaceBufferLock(screenSurface, 3);

        [view.layer setContents: screenSurface];

        CoreSurfaceBufferUnlock(screenSurface);

        objc_property_attribute_t type = { "T", "@\"CADisplayLink\"" };
        objc_property_attribute_t ownership = { "C", "" }; // C = copy
        objc_property_attribute_t backingivar  = { "V", "_privateName" };
        objc_property_attribute_t attrs[] = { type, ownership, backingivar };
        class_addProperty(SDLView.class, "")
        //CGContextRef myContext = [[NSGraphicsContext currentContext] graphicsPort];

        //CGBitmapContextCreate()

    }
}
#endif

VoodooDrawState attachDisplayLinkedPixelRendererToWindow(SDL_Window* window, size_t width, size_t height) {
    SDL_SysWMinfo info;
    if (!getWMInfo(window, &info)) {
        VoodooDrawState ret = {.screenSurface = NULL};
        return ret;
    }
    
    @autoreleasepool {
        NSWindow* nsWindow = info.info.cocoa.window;
        NSView* view = /*(SDLView*)*/nsWindow.contentView; // https://developer.apple.com/documentation/appkit/nswindow/1419160-contentview?language=objc
        
        CustomView* customView = [[CustomView alloc] initWithFrame: [view frame] pixelBufferWidth: width pixelBufferHeight: height];
        [view addSubview: customView];
        
        /* objc_property_attribute_t type = { "T", "@\"CADisplayLink\"" };
        objc_property_attribute_t ownership = { "C", "" }; // C = copy
        objc_property_attribute_t backingivar  = { "V", "_privateName" };
        objc_property_attribute_t attrs[] = { type, ownership, backingivar };
        class_addProperty(SDLView.class, "") */
        //CGContextRef myContext = [[NSGraphicsContext currentContext] graphicsPort];
        
        //CGBitmapContextCreate()
        
        
        VoodooDrawState ret = {.screenSurface = customView->pixelBuffer};
        return ret;
    }
}
