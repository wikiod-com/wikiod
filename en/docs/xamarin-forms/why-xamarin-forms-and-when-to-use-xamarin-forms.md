---
title: "Why Xamarin Forms and When to use Xamarin Forms"
slug: "why-xamarin-forms-and-when-to-use-xamarin-forms"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

You can refer to the official Xamarin Forms documentation to explore more:

https://www.xamarin.com/forms

## Why Xamarin Forms and When to use Xamarin Forms
Xamarin is becoming more and more popular - it is hard to decide when to use Xamarin.Forms and when Xamarin.Platform (so Xamarin.iOS and Xamarin.Android).

**First of all you should know for what kind of applications you can use Xamarin.Forms:**

1. Prototypes - to visualize how your application will look on the different devices.

2. Applications which not require platform specific functionality (like APIs) - but here please note that Xamarin is working busily to provide as many cross-platform compatibility as possible.

3. Applications where code sharing is crucial - more important than UI.

4. Applications where data displayed is more important than advanced functionality

**There are also many other factors:**

1. Who will be responsible for application development - if your team consists of experienced mobile developers they will be able to handle Xamarin.Forms easily. But if you have one developer per platform (native development) Forms can be bigger challenge.

2. Please also note that with Xamarin.Forms you can still encounter some issues sometimes - Xamarin.Forms platform is still being improved.

3. Fast development is sometimes very important - to reduce costs and time you can decide to use Forms.

4. When developing enterprise applications without any advanced functionality it is better to use Xamarin.Forms - it enables you to share mode code not event in mobile area but in general. Some portions of code can be shared across many platforms.

**You should not use Xamarin.Forms when:**

1. You have to create custom functionality and and access platform specific APIs

2. You have to create custom UI for the mobile application

3. When some functionality is not ready for Xamarin.Forms (like some specific behaviour on the mobile device)

4. Your team consists of platform specific mobile developers (mobile development in Java and/or Swift/Objective C)



