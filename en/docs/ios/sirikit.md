---
title: "SiriKit"
slug: "sirikit"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

# Different types of Siri requests

* Ride Booking (e.g. Get me a ride to New York via MyApp)

* Messaging (e.g. Send a text to John using MyApp)

* Photo Search (e.g. Look for beach photos taken last summer in MyApp)

* Payments (e.g. Send $20 to John for dinner last night using MyApp)

* VoIP Calling (e.g. Call Mike on my MyApp)

* Workouts (e.g. Start my daily run workout from MyApp)

* Climate and Radio (specifically designed for CarPlay, e.g. Set the heater to 72 degrees)

## Adding Siri Extension to App
To integrate Siri capabilities in your app, you should add an extensions as you would do while creating an iOS 10 Widget (old Today View Extension) or a custom keyboard.

# Adding capability

1- In the project settings, select your iOS app target and go to Capabilities tab

2- Enable the Siri capability

# Adding the extension

1- Go to File -> New -> Target...

2- Select iOS -> Application Extension from the left pane

3- Double-click Intents Extension from right

> ## According to Apple:
> Intents Extension template builds an Intents extension that allows your app to handle intents issued by system services like Siri and Maps.

[![enter image description here][1]][1]

4- Choose a name, and be sure to check "Include UI Extension"

[![enter image description here][2]][2]

By doing this steps, two new targets (Intents Extension and UI Extension) are created, and by default they contain Workout Intent code. For different types of Siri requests, see Remarks.

> # Note
> Anytime you want to debug your extension, just select the Intent scheme from the available schemes.

> # Note
> You can't test SiriKit apps in the Simulator. Instead, you need a real device.



  [1]: http://i.stack.imgur.com/fWg8H.png
  [2]: http://i.stack.imgur.com/YGgzc.png

