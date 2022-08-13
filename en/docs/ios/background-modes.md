---
title: "Background Modes"
slug: "background-modes"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

Being responsive is a need for every app. Users want to have apps which have their content ready when they open it, so developers should use Background Modes to make their apps more user friendly.

## Turning on the Background Modes capability
1. Go to Xcode and open your project.

2. In your app target, navigate to Capabilities tab.

3. Turn on Background Modes.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/5Fj8R.png

## Background Fetch
Background fetch is a new mode that lets your app appear always up-to-date with the latest information while minimizing the impact on battery. You could download feeds within fixed time intervals with this capability.

To get started:

1- Check Background Fetch in capabilities screen in Xcode.

2- In `application(_:didFinishLaunchingWithOptions:)` method in `AppDelegate`, add:

# Swift

    UIApplication.shared.setMinimumBackgroundFetchInterval(UIApplicationBackgroundFetchIntervalMinimum)

# Objective-C

    [[UIApplication shared] setMinimumBackgroundFetchInterval:UIApplicationBackgroundFetchIntervalMinimum]

>Instead of `UIApplicationBackgroundFetchIntervalMinimum`, you could use any `CGFloat` value to set fetch intervals.

3- You must implement `application(_:performFetchWithCompletionHandler:)`. Add that to your `AppDelegate`:

# Swift

    func application(_ application: UIApplication, performFetchWithCompletionHandler completionHandler: @escaping (UIBackgroundFetchResult) -> Void) {
        // your code here
    }

## Testing background fetch
1- Run the app on a real device and attach it to Xcode debugger.

2- From Debug menu, select **Simulate Background Fetch**:

[![enter image description here][1]][1]

3- Now Xcode will pause the app with SIGSTOP signal. Just tap the continue button to let the app do the background fetch.

[![enter image description here][2]][2]

Now you will see that data is fetched and ready for you.


  [1]: https://i.stack.imgur.com/OE4dg.png
  [2]: https://i.stack.imgur.com/wWxSS.png

## Background Audio
By default, when you are streaming an audio, by exiting the app it will stop, but you can prevent this by turning on the first check box in Background capability page in Xcode.

iOS will automatically handle this for you, and you don't need to write any code!

