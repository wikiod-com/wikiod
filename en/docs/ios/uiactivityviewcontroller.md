---
title: "UIActivityViewController"
slug: "uiactivityviewcontroller"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Parameters
| Parameter Name | Description |
| ------ | ------ |
| activityItems   | Contains array of object to perform the activity. This array must not be nil and must contain at least one object. |
| applicationActivities  | An array of UIActivity objects representing the custom services that your application supports. This parameter can be nil.      |

## Initializing the Activity View Controller
## **Objective-C**

    NSString *textToShare = @"StackOverflow Documentation!! Together, we can do for Documentation what we did for Q&A.";
    NSURL *documentationURL = [NSURL URLWithString:@"http://stackoverflow.com/tour/documentation"];
    
    NSArray *objectsToShare = @[textToShare, documentationURL];
    
    UIActivityViewController *activityVC = [[UIActivityViewController alloc] initWithActivityItems:objectsToShare applicationActivities:nil];
    
    [self presentViewController:activityVC animated:YES completion:nil];
    
## **Swift**

    let textToShare = "StackOverflow Documentation!! Together, we can do for Documentation what we did for Q&A."
    let documentationURL = NSURL(string:"http://stackoverflow.com/tour/documentation")

    let objToShare : [AnyObject] = [textToShare, documentationURL!]

    let activityVC = UIActivityViewController(activityItems: objToShare, applicationActivities: nil)
    self.presentViewController(activityVC, animated: true, completion: nil)

