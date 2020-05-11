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

#define sizeof_Color sizeof(Color)

static bool Running = true;

@interface HandmadeMainWindowDelegate: NSObject<NSWindowDelegate>
@end

@implementation HandmadeMainWindowDelegate

- (void)windowWillClose:(id)sender {
  Running = false;
}

@end

// The values of this enum are the number of bits per r,g,b, etc. component.
typedef enum ColorResolution {
    _16BitsPerPixel = 4, // Hopefully 5 bits red, 6 bits green, 5 bits blue. (5 + 6 + 5 = 16 bits per pixel.)
    _32BitsPerPixel = 8 // RGBA (or RGBX which would mean alpha is ignored, not just RGB since it would use 24 bits per pixel which is worse for performance than aligning to 32-bit boundaries.)
} ColorResolution;

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
        
        // Prepare pixels //
        // Greyscale test
        size_t size = SCREEN_WIDTH * SCREEN_HEIGHT * sizeof_Color;
        for(size_t ui = 0; ui < size; ui++) {
            view->pixelBuffer[ui] = 210;
        }
        // //
        
        view->pixelBuffer[6] = 2; // This actually affects it! Wow.
        
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
        
        NSDate* distantPast = [NSDate distantPast];
        while(Running) {
            @autoreleasepool { // This inner autoreleasepool will free these resources each frame.
            NSEvent* Event;
            
            // Run the "run loop" for a bit.
            do {
                Event = [NSApp nextEventMatchingMask: NSEventMaskAny
                                           untilDate: distantPast //distantPast prevents blocking; nil may cause a delay? ( https://stackoverflow.com/questions/985035/anyone-know-why-nexteventmatchingmaskuntildateinmodedequeue-take-many-ms-to )
                                              inMode: NSDefaultRunLoopMode
                                             dequeue: YES];
                
                switch ([Event type]) {
                    default:
                        [NSApp sendEvent: Event];
                }
            } while (Event != nil);
            }
            
            // Render again
            [view setNeedsDisplay: YES];
            //[unnecesary according to https://developer.apple.com/documentation/appkit/nswindow/1419609-viewsneeddisplay?language=objc :] [window setViewsNeedDisplay: YES];
            
            // Delay for 15 milliseconds, or (15 * 1000) microseconds.
            usleep(15 * 1000);
        }
    }
    return 0;
}


#endif // USE_CUSTOM_RENDERER_EEPIXELVIEWER
