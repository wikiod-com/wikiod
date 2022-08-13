---
title: "NSStoryBoard"
slug: "nsstoryboard"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Open a New Window Controller
To open a new window, add the following code somewhere where you can keep a reference to the new window (I.E., the app delegate).

Swift

    let storyboard:NSStoryboard = NSStoryboard(name: "Main", bundle: nil)
    guard let controller:NSWindowController = storyboard.instantiateControllerWithIdentifier("myWindowController") as? NSWindowController else { return /*or handle error*/ }
    controller.showWindow(self)

Objective-C

    NSStoryboard *storyBoard = [NSStoryboard storyboardWithName:@"Main" bundle:nil]; // get a reference to the storyboard
    myController = [storyBoard instantiateControllerWithIdentifier:@"secondWindowController"]; // instantiate your window controller
    [myController showWindow:self];


Once you create your `controller` make sure you keep a reference it to somewhere outside of the function call.  This can be done by creating a `NSWindowController` variable in your app delegate, and assigning your new controller to the variable.

