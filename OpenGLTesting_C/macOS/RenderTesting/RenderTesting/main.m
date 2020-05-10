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
#define sizeof_Color 4

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

// [createBuffer: Private: Callbacks for Core Graphics] //

// Gives Core Graphics access to the data to be used in the CGImage for the CGDataProvider that is "direct".
// "info" is "the same pointer you supplied to CGDataProviderCreateDirectAccess." ( https://developer.apple.com/documentation/coregraphics/cgdataprovidergetbytepointercallback?language=objc )
const void * _Nullable getBytePointer(void *info) {
    return info;
}

// "CGDataProviderReleaseBytePointerCallback
// A callback function that releases the pointer Core Graphics obtained by calling CGDataProviderGetBytePointerCallback."
void releaseBytePointer(void *info, const void *pointer) {
    // Do nothing, because we don't need this
    printf("Free\n"); // "You must not move or modify the provider data until Core Graphics calls your CGDataProviderReleaseBytePointerCallback function."
}

/* // https://developer.apple.com/documentation/coregraphics/cgdataprovidergetbytesatpositioncallback?language=objc
size_t getBytesAtPosition(void *info, void *buffer, off_t pos, size_t cnt) {
    // Not sure if this works:
    memcpy(buffer + pos, info, cnt);
    return cnt;
} */

// https://developer.apple.com/documentation/coregraphics/cgdataproviderdirectcallbacks?language=objc , "For the callback to work, one of the getBytePointer and getBytesAtPosition parameters must be non-NULL. If both are non-NULL, then getBytePointer is used to access the data."
CGDataProviderDirectCallbacks callbacks = {
    .getBytePointer = getBytePointer,
    .getBytesAtPosition = NULL,
    .releaseBytePointer = releaseBytePointer,
    // "CGDataProviderReleaseInfoCallback
    // A callback function that releases any private data or resources associated with the data provider."
    .releaseInfo = NULL,
    .version = 0 // "The version of this structure. It should be set to 0." ( https://developer.apple.com/documentation/coregraphics/cgdataproviderdirectcallbacks?language=objc )
};

// //

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
    
    //CFDataRef rgbData = CFDataCreateWithBytesNoCopy(NULL, pixelData, size, NULL); // Last argument is: "The allocator to use to deallocate the external buffer when the CFData object is deallocated. If the default allocator is suitable for this purpose, pass NULL or kCFAllocatorDefault. If you do not want the created CFData object to deallocate the buffer (that is, you assume responsibility for freeing it yourself), pass kCFAllocatorNull." ( https://developer.apple.com/documentation/corefoundation/1541971-cfdatacreatewithbytesnocopy?language=objc )
    
    CGDataProviderRef provider = CGDataProviderCreateDirect(pixelData, size, &callbacks);
    
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
    
    //CFRelease(rgbData);
    
    CGDataProviderRelease(provider); // [...]"Release" decrements the retain count.
    
    CGColorSpaceRelease(colorspace);
    
    // Now use the created CGImage. Must be relased with CGImageRelease(rgbImageRef).
    return rgbImageRef;
}


@interface CustomView: NSView {
@public
    CGImageRef imageRef;
    UInt8* pixelData;
    size_t counter;
}
@end

@implementation CustomView: NSView
- (BOOL)opaque {
    return YES; // Performance boost
}

// https://www.objc.io/issues/14-mac/appkit-for-uikit-developers/ : "But in AppKit, you shouldn’t touch the layer. If you want to interact with the layer in such ways, then you have to go one step further. Overriding NSView‘s wantsUpdateLayer method to return YES enables you to change the layer’s properties. If you do this though, AppKit will no longer call the view’s drawRect: method. Instead, updateLayer will be called during the view update cycle, and this is where you can modify the layer."
- (void)drawRect:(NSRect)dirtyRect {
    size_t size = SCREEN_WIDTH * SCREEN_HEIGHT * sizeof_Color;
    for(size_t ui = 0; ui < size; ui++) {
        pixelData[ui] = 1 + counter; // [Update: strangely, this stops working after we render the image...] This also works!... So does this mean we are using software rendering? Wow...
    }
    
    counter += 15;
    
    // https://stackoverflow.com/questions/3424103/drawing-image-with-cgimage
    CGContextRef ctx = [NSGraphicsContext currentContext].CGContext; // Get the current Core Graphics context being used for this drawRect function.
    CGContextDrawImage(ctx, dirtyRect, imageRef); // Draw our image.
}
@end

// https://medium.com/@theobendixson/handmade-hero-osx-platform-layer-day-2-b26d6966e214
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        HandmadeMainWindowDelegate *mainWindowDelegate = [[HandmadeMainWindowDelegate alloc] init];
        
        // Prepare pixels //
        // Greyscale test
        size_t size = SCREEN_WIDTH * SCREEN_HEIGHT * sizeof_Color;
        UInt8* pixelData = malloc(size);
        for(size_t ui = 0; ui < size; ui++) {
            pixelData[ui] = 210;
        }
        
        CGImageRef img = createBuffer(pixelData, SCREEN_WIDTH, SCREEN_HEIGHT, _32BitsPerPixel);
        // //
        
        pixelData[6] = 2; // This actually affects it! Wow.

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
        view->pixelData = pixelData;
        view->counter = 0;
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
        
        view->imageRef = nil;
        CGImageRelease(img); // Don't need to release the memory for pixelData since we passed NULL as the last argument to CFDataCreateWithBytesNoCopy.
    }
    return 0;
}
