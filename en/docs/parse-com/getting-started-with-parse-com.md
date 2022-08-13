---
title: "Getting started with parse.com"
slug: "getting-started-with-parsecom"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
<h1> Install the SDK </h1>

1. **Download & unzip the SDK**

Make sure you are using the latest version of Xcode (7.0+) and targeting iOS 7.0 or highe

[Download SDK][1]


2. **Add the SDKs to your app**

Drag the `Parse.framework` and `Bolts.framework` you downloaded into your Xcode project folder target.
Make sure the *Copy items to destination's group folder* checkbox is checked.

[![parse][2]][2]

3. **Add the dependencies**
Click on Targets → Your app name → and then the 'Build Phases' tab.
Expand 'Link Binary With Libraries' as shown.

[![parse][3]][3]

Click the + button in the bottom left of the 'Link Binary With Libraries' section and add the following libraries:

- AudioToolbox.framework
- CFNetwork.framework
- CoreGraphics.framework
- CoreLocation.framework
- QuartzCore.framework
- Security.framework
- StoreKit.framework
- SystemConfiguration.framework
- libz.tbd
- libsqlite3.tbd
>**Note**: This is a comprehensive list of dependencies for a typical app. You may be able to omit some of these if you are not using the -ObjC linker flag or if you do not plan to implement Location Services or In-App Purchases, for example


**Other installation options**

**CocoaPods**

Add `pod 'Parse'` to your podfile and run pod install.

**Compiling for yourself**

If you want to manually compile the SDK, you can find the source code on GitHub.

[GitHub link][4]

--- 
<h1> Connect your app to Parse Server </h1>

Open up your AppDelegate.m and add the following to it:

    #import <Parse/Parse.h>
 
    @implementation AppDelegate
 
     - (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
 
        // Initialize Parse.
        [Parse initializeWithConfiguration:[ParseClientConfiguration configurationWithBlock:^(id<ParseMutableClientConfiguration> configuration) {
           configuration.applicationId = @"YOUR_APP_ID";
           configuration.server = @"http://YOUR_PARSE_SERVER:1337/parse";
         }]];
 
      // ...
    }
 
    // ...


--- 

<h1>Test the SDK</h1>

First make sure to include our SDK libraries from your `.h` file:

    #import <Parse/Parse.h>

Then copy and paste this code into your app, for example in the viewDidLoad method (or inside another method that gets called when you run your app):

    PFObject *testObject = [PFObject objectWithClassName:@"TestObject"];
    testObject[@"foo"] = @"bar";
    [testObject saveInBackground];

Run your app. A new object of class TestObject will be sent to the Parse Server and saved.


  [1]: https://www.parse.com/downloads/ios/parse-library/latest "Download latest SDK"
  [2]: http://i.stack.imgur.com/lQ1YB.png
  [3]: http://i.stack.imgur.com/Gth5r.png
  [4]: https://github.com/ParsePlatform/parse-sdk-ios-osx

