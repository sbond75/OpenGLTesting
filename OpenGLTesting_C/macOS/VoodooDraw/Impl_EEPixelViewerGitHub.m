#import "Impl_EEPixelViewerGitHub.h"

@implementation CustomView

- (id)initWithFrame:(CGRect)rect pixelBufferWidth:(size_t)width pixelBufferHeight:(size_t)height {

    self = [ super initWithFrame: rect ];
    if (nil != self) {
        self.fpsIndicator = YES;
        self.pixelFormat = kCVPixelFormatType_32RGBA;
        self.sourceImageSize = CGSizeMake(width, height);
        plane.width = width; //1024;
        plane.height = height; //768;
        plane.rowBytes = plane.width * 4;
        pixelBuffer = malloc(plane.rowBytes * plane.height);
        plane.data = pixelBuffer;
        
        counter = 0;
        
        [self setupShadersForCropAndScaling];
        
        [self setupDisplayLink];
    }

    return self;
}

- (void)dealloc {
    CVDisplayLinkStop(displayLink);
    free(pixelBuffer);
}

static CVReturn renderCallback(CVDisplayLinkRef displayLink,
                               const CVTimeStamp *inNow,
                               const CVTimeStamp *inOutputTime,
                               CVOptionFlags flagsIn,
                               CVOptionFlags *flagsOut,
                               void *displayLinkContext)
{
    return [(__bridge CustomView *)displayLinkContext renderCallback:inOutputTime];
}

-(void)setupDisplayLink {
    // Synchronize buffer swaps with vertical refresh rate
    GLint swapInt = 1;
    [[self openGLContext] setValues:&swapInt forParameter:NSOpenGLCPSwapInterval];
    
    CGDirectDisplayID   displayID = CGMainDisplayID();
    CVReturn            error = kCVReturnSuccess;
    error = CVDisplayLinkCreateWithCGDisplay(displayID, &displayLink);
    if (error)
    {
        NSLog(@"DisplayLink created with error:%d", error);
        displayLink = NULL;
    }
    CVDisplayLinkSetOutputCallback(displayLink, renderCallback, (__bridge void *)self);
    
    // Set the display link for the current renderer
    CGLContextObj cglContext = [[self openGLContext] CGLContextObj];
    CGLPixelFormatObj cglPixelFormat = [self.openGLContext.pixelFormat CGLPixelFormatObj];
    CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext(displayLink, cglContext, cglPixelFormat);
    
    // Activate the display link
    CVDisplayLinkStart(displayLink);
}

// This function should be invoked on the main thread:
-(void)displayPixelBuf {
    /* [self setWantsLayer: YES];
    [self.layer setDrawsAsynchronously:YES]; */
    
    // Greyscale test
    size_t size = plane.rowBytes * plane.height;
    for(size_t ui = 0; ui < size; ui++) {
        self->pixelBuffer[ui] = 1+ui+counter;
    }
    
    [self displayPixelBufferPlanes: &plane count: 1];
    //[self.openGLContext flushBuffer];
    //[self setNeedsDisplay:YES];
    
}

// Gets the frame for a given time, inOutputTime. ( https://developer.apple.com/library/archive/qa/qa1385/_index.html )
-(CVReturn)renderCallback: (const CVTimeStamp*)inOutputTime {
    counter += 20;
    
    // https://medium.com/@eyeplum/cvdisplaylink-a0f878f8f053
    [self performSelectorOnMainThread:@selector(displayPixelBuf) withObject:nil waitUntilDone:NO];
    return kCVReturnSuccess;
}

@end
