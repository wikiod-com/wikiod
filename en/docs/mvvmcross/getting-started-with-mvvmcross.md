---
title: "Getting started with mvvmcross"
slug: "getting-started-with-mvvmcross"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What's mvvmcross
Mvvmcross is an open source MVVM platform that comes to solve our problems while developing cross platform applications (such as Android, Windows Phone, IOS, etc...).

The main problem is, that every platform has its own specifics, but the business logic of your application is likely to be the same on each platform.

For instance, if you develop your application for Android and Windows Desktop - your mobile project will have a vibration API while desktop will not. But both desktop and mobile project will have the same authentication mechanism.

That’s where Mvvmcross becomes very handy.

The main feature of mvvmcross is that you are separating your logic not only from the views (as in MVVM and MVC patterns), but from specific platform implementation while reusing logic code between platforms.

Your business logic will be implemented in your PCL (Portable Library Class), and each platform can reference it, while implementing its native side accordingly.

Check out the mvvmcross manifesto:

https://github.com/MvvmCross/MvvmCross/wiki/The-MvvmCross-Manifesto

Mvvmcross on Github:

https://github.com/MvvmCross/MvvmCross

For those that are completely new to MVC/MVVM concept, I suggest you visit these links:

http://en.wikipedia.org/wiki/Model_View_ViewModel
http://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller
v

## Installation or Setup
Detailed instructions on getting mvvmcross set up or installed.

## MvvmCross Installation
**Installing mvvmcross with nugget:**

Search for mvvmcross in the "Manage Nugget Packages" window.

**Installing mvvmcross with Package Manger Console:**

    PM> Install-Package MvvmCross

* Make sure to install it on both your PCL (Portable Class Library) and you application project.

As the nugget finishes its installation, notice that you have two new directories named "ToDo-MvvmCross" in your PCL and the application Project, follow the instructions step by step.

If you done everything right, you can compile and run your application. You should see this "Hello MvvmCross" screen: 

[![enter image description here][1]][1]



  [1]: http://i.stack.imgur.com/tJtrU.png

