//
//  main.m
//  RenderTesting
//
//  Created by sbond75 on 5/8/20.
//  Copyright © 2020 sbond75. All rights reserved.
//
// https://github.com/TheoBendixson/Handmade-Hero-MacOS-Platform-Layer-Non-Video/blob/master/Day002/handmade/code/osx_main.mm

#include <AppKit/AppKit.h>
#include "CoreSurface.h"
#include <CoreGraphics/CoreGraphics.h>

// Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

static bool Running = true;

#if 0
CoreSurfaceBufferRef createBuffer(int width, int height) {
    CFMutableDictionaryRef dict;
    int x = width, y = height, pitch = x * 2, size = 2 * x * y;
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
#endif

@interface HandmadeMainWindowDelegate: NSObject<NSWindowDelegate>
@end

@implementation HandmadeMainWindowDelegate

- (void)windowWillClose:(id)sender {
  Running = false;
}

@end

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

        [window setBackgroundColor: NSColor.redColor];
        //[window setTitle: @"Handmade Hero"];
        [window makeKeyAndOrderFront: nil];
        [window setDelegate: mainWindowDelegate];
        //window.contentView.wantsUpdateLayer = YES;
        NSLog(@"%d", window.contentView);
        NSLog(@"%d", window.contentView.opaque); // Set to YES for high performance
        NSLog(@"%d", window.opaque); // Set to YES for high performance?
        
        //CoreSurfaceBufferRef screenSurface = createBuffer(SCREEN_WIDTH, SCREEN_HEIGHT);
        //CoreSurfaceBufferLock(screenSurface, 3);

        while(Running) {
            NSEvent* Event;
            
            do {
                Event = [NSApp nextEventMatchingMask: NSEventMaskAny
                                           untilDate: nil
                                              inMode: NSDefaultRunLoopMode
                                             dequeue: YES];
                
                switch ([Event type]) {
                    default:
                        [NSApp sendEvent: Event];
                }
            } while (Event != nil);
        }
    }
    return 0;
}
