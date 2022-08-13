---
title: "Snapshot of UIView"
slug: "snapshot-of-uiview"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Getting the Snapshot
    - (UIImage *)getSnapshot 
    {      
    UIScreen *screen = [UIScreen mainScreen];
    CGRect bounds = [self.view bounds];
    UIGraphicsBeginImageContextWithOptions(bounds.size, false, screen.scale);
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetInterpolationQuality(context, kCGInterpolationHigh);
    [self.view drawViewHierarchyInRect:bounds afterScreenUpdates:YES];
    UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();  
    return image;
    }

**Swift**

    var screenshot: UIImage
    {
    UIGraphicsBeginImageContext(self.bounds.size);
    let context = UIGraphicsGetCurrentContext();
    self.layer.render(in: context)
    let screenShot = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return screenShot
    }

## Snapshot with subview with other markup and text
 - Support Portrait and Landscape both type of image
 - Drawing and other subviews can be merged in my case I'm adding label to draw



    {
        CGSize fullSize = getImageForEdit.size;
        CGSize sizeInView = AVMakeRectWithAspectRatioInsideRect(imgViewFake.image.size, imgViewFake.bounds).size;
        CGFloat orgScale = orgScale = fullSize.width/sizeInView.width;
        CGSize newSize = CGSizeMake(orgScale * img.image.size.width, orgScale * img.image.size.height);
        if(newSize.width <= fullSize.width && newSize.height <= fullSize.height){
            newSize = fullSize;
        }
        CGRect offsetRect;
        if (getImageForEdit.size.height > getImageForEdit.size.width){
            CGFloat scale = newSize.height/fullSize.height;
            CGFloat offset = (newSize.width - fullSize.width*scale)/2;
            offsetRect = CGRectMake(offset, 0, newSize.width-offset*2, newSize.height);
        }
        else{
            CGFloat scale = newSize.width/fullSize.width;
            CGFloat offset = (newSize.height - fullSize.height*scale)/2;
            offsetRect = CGRectMake(0, offset, newSize.width, newSize.height-offset*2);
        }
        UIGraphicsBeginImageContextWithOptions(newSize, NO, getImageForEdit.scale);
        [getImageForEdit drawAtPoint:offsetRect.origin];
        //        [img.image drawInRect:CGRectMake(0,0,newSize.width,newSize.height)];
        CGFloat oldScale = img.contentScaleFactor;
        img.contentScaleFactor = getImageForEdit.scale;
        [img drawViewHierarchyInRect:CGRectMake(0, 0, newSize.width, newSize.height) afterScreenUpdates:YES];
        img.contentScaleFactor = oldScale;
        UIImage *combImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        imageData = UIImageJPEGRepresentation(combImage, 1);
    }



