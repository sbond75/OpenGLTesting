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

// pixelData is of size (width * height * numberOfBytesPerPixel).
// The returned image must be freed with CGImageRelease().
CGImageRef createBuffer(UInt8* pixelData, size_t width, size_t height, ColorResolution colorRes) {
    /*
     https://developer.apple.com/documentation/coregraphics/1455149-cgimagecreate?language=objc
     https://stackoverflow.com/questions/6861891/how-do-you-generate-an-rgba-image-in-core-graphics
     https://www.raywenderlich.com/2335-image-processing-in-ios-part-1-raw-bitmap-modification
     */
    size_t numBytesPerPixel = (size_t)colorRes / 2;
    assert((width * numBytesPerPixel) % 16 == 0); // Else, bad performance. ( https://developer.apple.com/library/archive/documentation/GraphicsImaging/Conceptual/drawingwithquartz2d/dq_context/dq_context.html#//apple_ref/doc/uid/TP30001066-CH203-BCIBHHBB says "When you create a bitmap graphics context, you’ll get the best performance if you make sure the data and bytesPerRow are 16-byte aligned.")
    
    //CGContextRef context = CGBitmapContextCreateWithData(NULL, SCREEN_WIDTH, SCREEN_HEIGHT, kCGImageAlphaNoneSkipLast, pitch, colorSpace, kCGImageAlphaNoneSkipLast, NULL, NULL);
    
    const size_t size = width * height * numBytesPerPixel;
    
    CGColorSpaceRef colorspace = CGColorSpaceCreateDeviceRGB();
    
    CFDataRef rgbData = CFDataCreateWithBytesNoCopy(NULL, pixelData, size, NULL);
    
    CGDataProviderRef provider = CGDataProviderCreateWithCFData(rgbData);
    
    size_t bitsPerComponent = (size_t)colorRes;
    size_t bitsPerPixel = numBytesPerPixel * 8;
    assert(bitsPerPixel == bitsPerComponent * 4);
    size_t pitch = width * numBytesPerPixel; // This is equivalent to "bytesPerRow".
    CGBitmapInfo bitmapInfo;
    switch (colorRes) {
    case _16BitsPerPixel:
        bitmapInfo = kCGBitmapByteOrder16Little;
        break;
    case _32BitsPerPixel:
        bitmapInfo = kCGBitmapByteOrder32Little | kCGImageAlphaNoneSkipLast; // RGBX, where X is skipped.
        break;
    default:
        assert(0);
    }
    CGColorRenderingIntent intent = kCGRenderingIntentDefault;
#define decodeArray NULL // (const CGFloat*) Can use to remap color values.
#define shouldInterpolate 0 // false
    CGImageRef rgbImageRef = CGImageCreate(width, height, bitsPerComponent, bitsPerPixel, pitch, colorspace, bitmapInfo, provider, decodeArray, shouldInterpolate, intent);
#undef decodeArray
    
    CFRelease(rgbData);
    
    CGDataProviderRelease(provider);
    
    CGColorSpaceRelease(colorspace);
    
    // Now use the created CGImage. Must be relased with CGImageRelease(rgbImageRef).
    return rgbImageRef;
}


@interface CustomView: NSView {
@public
    CGImageRef imageRef;
}
@end

@implementation CustomView: NSView
- (BOOL)opaque {
    return YES; // Performance boost
}
- (void)drawRect:(NSRect)dirtyRect {
    // https://stackoverflow.com/questions/3424103/drawing-image-with-cgimage
    CGContextRef ctx = [NSGraphicsContext currentContext].CGContext;
    CGContextDrawImage(ctx, dirtyRect, imageRef);
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        HandmadeMainWindowDelegate *mainWindowDelegate = [[HandmadeMainWindowDelegate alloc] init];
        
        // Prepare pixels //
#define sizeof_Color 4
        // Greyscale test
        size_t size = SCREEN_WIDTH * SCREEN_HEIGHT * sizeof_Color;
        UInt8* pixelData = malloc(size);
        for(size_t ui = 0; ui < size; ui++) {
            pixelData[ui] = 210;
        }
        
        CGImageRef img = createBuffer(pixelData, SCREEN_WIDTH, SCREEN_HEIGHT, _32BitsPerPixel);
        // //
        

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
        
        CustomView* view = [[CustomView alloc] init];
        view->imageRef = img;
        [view setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
        [window setContentView: view]; // or [window addSubview]
        [window makeKeyAndOrderFront: nil];
        [window setDelegate: mainWindowDelegate];
        
        
        //window.contentView.wantsUpdateLayer = YES;
        //NSLog(@"%d", window.contentView); // -> Exists
        //NSLog(@"%d", window.contentView.opaque); // Set to YES for high performance
        //[is private:] window.contentView opaque = YES;
        //NSLog(@"%d", window.opaque); // Set to YES for high performance?
        
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
        
        view->imageRef = nil;
        CGImageRelease(img);
    }
    return 0;
}
