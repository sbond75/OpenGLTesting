#include <SDL2/SDL.h>
#import <Cocoa/Cocoa.h>

// Returns true if success, false if not.
// Fills the given struct.
bool getWMInfo(SDL_Window* window, SDL_SysWMinfo* wmInfo) {
#define info = *wmInfo
  // https://wiki.libsdl.org/SDL_GetWindowWMInfo

  SDL_VERSION(&info.version); /* initialize info structure with SDL version info */

  if(SDL_GetWindowWMInfo(window,&info)) { /* the call returns true on success */
    /* success */
    const char *subsystem = "an unknown system!";
    switch(info.subsystem) {
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
        (int)info.version.major,
        (int)info.version.minor,
        (int)info.version.patch,
        subsystem);
    return true;
  } else {
    /* call failed */
    SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Couldn't get window information: %s", SDL_GetError());
    return false;
  }
#undef info
}

// https://github.com/spurious/SDL-mirror/blob/master/src/video/cocoa/SDL_cocoawindow.m
/* @interface SDLView : NSView {
    SDL_Window *_sdlWindow;
} */

void createBuffer(size_t width, size_t height) {
#define BYTES_PER_PIXEL 3
    vImageBuffer buf;
    buf.data = malloc(BYTES_PER_PIXEL * width * height);
    buf.height = height;
    buf.width = width;
    buf.rowBytes = 
}

@interface CustomView : NSView {
    // Instance variables
    CADisplayLink* displayLink;
}
@implementation CustomView
-(void)createBuffer:  {
}
-(void)setupDisplayLink {
    self.link = [CADisplayLink displayLinkWithTarget: self, selector: @selector(drawStuff)];
    [link addToRunLoop: [NSRunLoop mainRunLoop], NSDefaultRunLoopMode];
}
-(void)drawStuff {
    self.layer.setNeedsDisplay();
}

-(void)drawLayer:(CALayer *)layer inContext:(CGContextRef)ctx {
    CALayer drawingLayer = self.drawingLayer ?? CAShapeLayer()
}
@end

bool test(SDL_Window* window) {
    SDL_SysWMinfo info;
    if (!getSDLInfo(window, info)) {
        return false;
    }

    NSWindow* window = info.cocoa.window;
    NSView* view = /*(SDLView*)*/window.contentView; // https://developer.apple.com/documentation/appkit/nswindow/1419160-contentview?language=objc
    

    //CGContextRef myContext = [[NSGraphicsContext currentContext] graphicsPort];

    //CGBitmapContextCreate()
}