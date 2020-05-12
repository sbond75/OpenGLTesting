//
//  main.m
//  Xcode
//
//  Created by sbond75 on 5/10/20.
//  Copyright © 2020 sbond75. All rights reserved.
//

#include "Config.h"
#ifdef USE_CUSTOM_RENDERER_EEPIXELVIEWER
#include <AppKit/AppKit.h>
#include "Misc.h"
#import "VoodooDraw/Impl_EEPixelViewerGitHub.h"
#import "VoodooDraw/TestOpenGLView/MyOpenGLView.h"

#define sizeof_Color sizeof(Color)

@interface HandmadeMainWindowDelegate: NSObject<NSWindowDelegate>
@end

@implementation HandmadeMainWindowDelegate

/* - (void)windowWillClose:(id)sender {
  Running = false;
} */

// https://developer.apple.com/documentation/appkit/nsapplicationdelegate/1428381-applicationshouldterminateafterl?language=objc
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)sender {
    return YES;
}

@end

// The values of this enum are the number of bits per r,g,b, etc. component.
typedef enum ColorResolution {
    _16BitsPerPixel = 4, // Hopefully 5 bits red, 6 bits green, 5 bits blue. (5 + 6 + 5 = 16 bits per pixel.)
    _32BitsPerPixel = 8 // RGBA (or RGBX which would mean alpha is ignored, not just RGB since it would use 24 bits per pixel which is worse for performance than aligning to 32-bit boundaries.)
} ColorResolution;

// https://developer.apple.com/documentation/corefoundation/cfrunloopobservercallback?language=objc
//void myRunLoopObserver(CFRunLoopObserverRef observer, CFRunLoopActivity activity, void *info) {
//    printf("myRunLoopObserver\n");
//}

// https://medium.com/@theobendixson/handmade-hero-osx-platform-layer-day-2-b26d6966e214
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        HandmadeMainWindowDelegate *mainWindowDelegate = [[HandmadeMainWindowDelegate alloc] init];

        NSRect screenRect = [[NSScreen mainScreen] frame];

        CGFloat w = (CGFloat)SCREEN_WIDTH;
        CGFloat h = (CGFloat)SCREEN_HEIGHT;
        NSRect initialFrame = NSMakeRect((screenRect.size.width - w) * 0.5,
                                         (screenRect.size.height - h) * 0.5,
                                         w,
                                         h);
        
        NSWindow *window = [[NSWindow alloc] initWithContentRect:initialFrame
                         styleMask: NSWindowStyleMaskTitled |
                                NSWindowStyleMaskClosable /* |
                                NSWindowStyleMaskMiniaturizable |
                                NSWindowStyleMaskResizable */
                         backing:NSBackingStoreBuffered
                         defer:NO];

        //[window setTitle: @"Handmade Hero"];
        
        CustomView* view = [[CustomView alloc] initWithFrame: initialFrame pixelBufferWidth: w pixelBufferHeight: h];
        //MyOpenGLView* view = [[MyOpenGLView alloc] initWithFrame: initialFrame];
        
#if 1
        // Prepare pixels //
        // Greyscale test
        size_t size = SCREEN_WIDTH * SCREEN_HEIGHT * sizeof_Color;
        for(size_t ui = 0; ui < size; ui++) {
            view->pixelBuffer[ui] = 255; //210;
        }
        // //
        
        view->pixelBuffer[6] = 2;
#endif
        
        [view setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
        [window setContentView: view]; // or [window addSubview]
        [window makeKeyAndOrderFront: nil];
        [window setDelegate: mainWindowDelegate];
        
        
        //window.contentView.wantsUpdateLayer = YES;
        //NSLog(@"%d", window.contentView); // -> Exists
        //NSLog(@"%d", window.contentView.opaque); // Set to YES for high performance
        //[is private:] window.contentView opaque = YES;
        //[was YES already:] NSLog(@"%d", window.opaque); // Set to YES for high performance?
        
        //CoreSurfaceBufferRef screenSurface = createBuffer(SCREEN_WIDTH, SCREEN_HEIGHT);
        //CoreSurfaceBufferLock(screenSurface, 3);
        
        //NSRunLoop *runLoop = [NSRunLoop currentRunLoop];
        
        // https://developer.apple.com/documentation/corefoundation/1541546-cfrunloopobservercreate?language=objc
        // Create a run loop observer and attach it to the run loop.
        //CFRunLoopObserverContext  context = {0, self, NULL, NULL, NULL}; //<-- any userdata you need.
        //CFRunLoopObserverRef    observer = CFRunLoopObserverCreate(kCFAllocatorDefault,
        //    kCFRunLoopExit /*kCFRunLoopAllActivities*/, YES, 0, &myRunLoopObserver, NULL /*&context*/);
        //CFRunLoopAddObserver([runLoop getCFRunLoop], observer, kCFRunLoopCommonModes);
        
        //[runLoop run];
        
        NSApplication *app = [NSApplication sharedApplication];
        [app setDelegate:mainWindowDelegate];
        [app run];
        
        // return NSApplicationMain(argc, argv); // Does the run loop above, etc. but fails in command line programs it seems: says "No Info.plist file in application bundle or no NSPrincipalClass in the Info.plist file, exiting"
    }
    return 0;
}


#endif // USE_CUSTOM_RENDERER_EEPIXELVIEWER
