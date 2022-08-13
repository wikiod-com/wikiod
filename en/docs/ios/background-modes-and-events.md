---
title: "Background Modes and Events"
slug: "background-modes-and-events"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Play Audio in Background
Add a key named **Required background modes** in property list (.plist) file ..

as following picture..

![enter image description here][1]

And add following code in 

**AppDelegate.h**

    #import <AVFoundation/AVFoundation.h>
    #import <AudioToolbox/AudioToolbox.h>

**AppDelegate.m**

in application didFinishLaunchingWithOptions

    [[AVAudioSession sharedInstance] setDelegate:self];
    [[AVAudioSession sharedInstance] setCategory:AVAudioSessionCategoryPlayback error:nil];
    [[AVAudioSession sharedInstance] setActive:YES error:nil];
    [[UIApplication sharedApplication] beginReceivingRemoteControlEvents];

    UInt32 size = sizeof(CFStringRef);
    CFStringRef route;
    AudioSessionGetProperty(kAudioSessionProperty_AudioRoute, &size, &route);
    NSLog(@"route = %@", route);

If you want changes as per events you have to add following code in AppDelegate.m 

    - (void)remoteControlReceivedWithEvent:(UIEvent *)theEvent {
        
        if (theEvent.type == UIEventTypeRemoteControl)    {
            switch(theEvent.subtype)        {
                case UIEventSubtypeRemoteControlPlay:
                    [[NSNotificationCenter defaultCenter] postNotificationName:@"TogglePlayPause" object:nil];
                    break;
                case UIEventSubtypeRemoteControlPause:
                    [[NSNotificationCenter defaultCenter] postNotificationName:@"TogglePlayPause" object:nil];
                    break;
                case UIEventSubtypeRemoteControlStop:
                    break;
                case UIEventSubtypeRemoteControlTogglePlayPause:
                    [[NSNotificationCenter defaultCenter] postNotificationName:@"TogglePlayPause" object:nil];
                    break;
                default:
                    return;
            }
        }
    }

Based on notification have to work on it..

  [1]: http://i.stack.imgur.com/m453q.png


