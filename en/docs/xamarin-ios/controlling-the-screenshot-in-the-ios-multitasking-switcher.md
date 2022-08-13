---
title: "Controlling the Screenshot in the iOS Multitasking Switcher"
slug: "controlling-the-screenshot-in-the-ios-multitasking-switcher"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

In the [App Programming Guide for iOS][1]:

Remove sensitive information from views before moving to the background.

When an app transitions to the background, the system takes a snapshot of the app’s main window, which it then presents briefly when transitioning your app back to the foreground. 


  [1]: https://developer.apple.com/library/content/documentation/iPhone/Conceptual/iPhoneOSProgrammingGuide/BackgroundExecution/BackgroundExecution.html#//apple_ref/doc/uid/TP40007072-CH4-SW8

Adapted from actual StackOverflow Question [Controlling the Screenshot in the iOS7 Multitasking Switcher][1] and answer [Obj-c Answer][2]


  [1]: http://stackoverflow.com/questions/18959411/controlling-the-screenshot-in-the-ios-7-multitasking-switcher/41409351#41409351
  [2]: http://stackoverflow.com/questions/18959411/controlling-the-screenshot-in-the-ios-7-multitasking-switcher/20040270#20040270

## Show an Image for Snapshot
    public override void DidEnterBackground(UIApplication application)
     {
        //to add the background image in place of 'active' image
        var backgroundImage = new UIImageView();
        backgroundImage.Tag = 1234;
        backgroundImage.Image = UIImage.FromBundle("Background");
        backgroundImage.Frame = this.window.Frame;
        this.window.AddSubview(backgroundImage);
        this.window.BringSubviewToFront(backgroundImage);
    }
    
    public override void WillEnterForeground(UIApplication application)
    {
        //remove 'background' image
        var backgroundView = this.window.ViewWithTag(1234);
        if(null != backgroundView)
            backgroundView.RemoveFromSuperview();
    }

