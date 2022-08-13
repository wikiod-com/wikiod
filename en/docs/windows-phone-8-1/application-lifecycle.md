---
title: "Application Lifecycle"
slug: "application-lifecycle"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Application LifeCycle Events and Methods (App.cs)
Every Windows Phone project contains App.cs class:

    public sealed partial class App : Application

This class is your global application context.

General **Application** class usage:

> 1. App entry point, particularly for various activation contracts.
> 2. Application lifecycle management.
> 3. Application global resources/services initialization.
> 4. Unhandled exception detection.

**Application Life Cycle Events**

In **App.cs** you can subscribe to life cycle events: Suspending, Resuming

*Suspending*
 
Raised when your application state changed to Suspending state. 
Suspending state means that your app isn't visible by the user (occurs when user switches to another app), **it's the only indication that you get before your app is terminated**.
That's why this is where you'll want to save your session state, such as movie position that currently played, or some other valuable state information).

* **Application termination isn't deterministic**, the OS can shut down suspended application at any time.

*Resuming* 

Raised while when your application is back from Suspending mode.
The OS restores the app state automatically (your page that you've been on when application Suspended), this is where you should restore your application state that could be changed by the time your app was suspended.

* You should handle the Resuming event if you need to refresh any displayed content that might have changed while the app was suspended.


**Application virtual methods**

Also you have two virtual methods that you can override:

    protected virtual void OnActivated(IActivatedEventArgs args);
    protected virtual void OnLaunched(LaunchActivatedEventArgs args);

*OnActivated* 

Called when the application is activated somehow (not normal launching).
You won't get here if you simply launch the app by clicking a tile. 
For example, you'll get here when another app launches your app.
(You can the **IActivatedEventArgs.Kind** for activation reason).

*OnLaunched* 

Invoked when the application is launched. This method will be called every time your application launched. Generally used as main point for application initialization.




