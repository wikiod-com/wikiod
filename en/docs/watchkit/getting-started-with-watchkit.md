---
title: "Getting started with watchkit"
slug: "getting-started-with-watchkit"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Connecting the code with the UI
Like iOS where you use `@IBOutlet` and `@IBAction`, here you could use them too.

Let's say we have a button which when clicked changes the label's text to something else.

To get started:

1. Add a `WKInterfaceLabel` and a `WKInterfaceLabel` to the `InterfaceController`.

2. Ctrl-Drag from the `WKInterfaceLabel` to `InterfaceController.swift` and enter the details as shown in the following picture to add an outlet property:

[![enter image description here][1]][1]

3. Ctrl-Drag from the `WKInterfaceButton` to `InterfaceController.swift` and enter the details as shown in the following picture to add an action method:

[![enter image description here][2]][2]

5. Fill the action method:

## Swift

    outputLabel.setText("Button Tapped!")

## Objective-C

    [[self outputLabel] setText:@"Button Tapped!"]

6. Run the program and tap the button to see the result.


  [1]: https://i.stack.imgur.com/7HbET.png
  [2]: https://i.stack.imgur.com/bHtyu.png

## Creating a new watchOS project
To develop an application for watchOS, you should start with Xcode. Xcode only runs on macOS. At the time of writing, the latest version is Xcode 8.3.

If you want to start a new project from scratch:

1. Boot up your Mac and install Xcode from the App Store if it's not already installed.

2. Choose to create a new project.

3. In templates, choose watchOS and then "iOS App with WatchKit App".

[![In templates, choose watchOS and then "iOS App with WatchKit App".][1]][1]

4. Fill your project details and choose a location.

If you already have an iOS project and want to add a watchOS target:

1. Go to File -> New -> Target.

[![Go to File -> New -> Target][2]][2]

2. Choose WatchKit App.

[![Choose WatchKit App][3]][3]

3. Fill your target details and choose a location. 


  [1]: https://i.stack.imgur.com/8j8FH.png
  [2]: https://i.stack.imgur.com/BcTlC.png
  [3]: https://i.stack.imgur.com/5lgdp.png

## Making a simple "Hello, World!" app
Each watchOS target includes an App and an Extension. App contains the UI stuff and Extension contains the actual logic (similar to Views and Models in MVC architecture in iOS).

Each WatchKit App has a `Interface.storyboard` file which you design the app in it, and a `Assets.xcassets` file to put your assets in.

Each WatchKit Extension has a `InterfaceController.swift` file (actually a `WKInterfaceController` subclass) which is similar to the `ViewController` file in iOS.

To make a Hello World app:

1. Open up the `Interface.storyboard`.

2. Locate the main `InterfaceController`.

[![Locate the main InterfaceController.][1]][1]

3. From the library in the right pane, add a `WKInterfaceLabel`.

[![From the library in the right pane, add a WKInterfaceLabel.][2]][2]

4. Drag the label and set its text in the right pane to "Hello, World!".

[![Drag the label and set its text in the right pane to "Hello, World!".][3]][3]

5. Select the correct scheme (according to the next picture), then run the project by either tapping the run button in the top bar, using Product menu, pressing Cmd-R or tapping run in the Touch Bar.

[![enter image description here][4]][4]

[![enter image description here][5]][5]

[![enter image description here][6]][6]

[![enter image description here][7]][7]

Apple Watch simulator will eventually show up with your app running.


  [1]: https://i.stack.imgur.com/CoFS3.png
  [2]: https://i.stack.imgur.com/KrEab.png
  [3]: https://i.stack.imgur.com/Uqvkk.png
  [4]: https://i.stack.imgur.com/6UEhc.png
  [5]: https://i.stack.imgur.com/HgScP.png
  [6]: https://i.stack.imgur.com/r5lVa.png
  [7]: https://i.stack.imgur.com/EOISs.png

