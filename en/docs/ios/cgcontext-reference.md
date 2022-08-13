---
title: "CGContext Reference"
slug: "cgcontext-reference"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

The **CGContextRef** opaque type represents a Quartz 2D drawing destination. A graphics context contains drawing parameters and all device-specific information needed to render the paint on a page to the destination, whether the destination is a window in an application, a bitmap image, a PDF document, or a printer.

## Draw line
    CGContextRef context = UIGraphicsGetCurrentContext();

    CGContextSetLineWidth(context, 5.0);
    CGColorSpaceRef colorspace = CGColorSpaceCreateDeviceRGB();
    CGContextMoveToPoint(context, 200, 400);
    CGContextAddLineToPoint(context, 100, 100);
    CGContextStrokePath(context);
    CGColorSpaceRelease(colorspace);

[![Line][1]][1]


  [1]: http://i.stack.imgur.com/9U48r.png


## Draw Text
Draw To requires **Core Text framework** to be added in the Build Phase


    [NSString* textToDraw = @"Welcome to the world Of IOS";
       
        CFStringRef stringRef = (__bridge CFStringRef)textToDraw;
        
        CFAttributedStringRef currentText = CFAttributedStringCreate(NULL, stringRef, NULL);
        CTFramesetterRef framesetter = CTFramesetterCreateWithAttributedString(currentText); 
        CGRect frameRect = CGRectMake(0, 0, 300, 100);
        CGMutablePathRef framePath = CGPathCreateMutable();
        CGPathAddRect(framePath, NULL, frameRect);
        
        CFRange currentRange = CFRangeMake(0, 0);
        CTFrameRef frameRef = CTFramesetterCreateFrame(framesetter, currentRange, framePath, NULL);
        CGPathRelease(framePath); 
        CGContextRef currentContext = UIGraphicsGetCurrentContext();
              
        CGContextSetTextMatrix(currentContext, CGAffineTransformIdentity);
        CGContextTranslateCTM(currentContext, 200, 300);
        CGContextScaleCTM(currentContext, 2, -2);
        CTFrameDraw(frameRef, currentContext);
        
        CFRelease(frameRef);
        CFRelease(stringRef);
        CFRelease(framesetter);

[![Text][1]][1]


  [1]: http://i.stack.imgur.com/23STV.png

