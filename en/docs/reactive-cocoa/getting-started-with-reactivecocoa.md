---
title: "Getting started with ReactiveCocoa"
slug: "getting-started-with-reactivecocoa"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
ReactiveCocoa supports macOS 10.9+, iOS 8.0+, watchOS 2.0+, and tvOS 9.0+.

Carthage
--------

If you use Carthage to manage your dependencies, simply add ReactiveCocoa to your Cartfile:

    github "ReactiveCocoa/ReactiveCocoa" ~> 5.0

If you use Carthage to build your dependencies, make sure you have added `ReactiveCocoa.framework`, `ReactiveSwift.framework`, and `Result.framework` to the "Linked Frameworks and Libraries" section of your target, and have included them in your Carthage framework copying build phase.

CocoaPods
---------

If you use CocoaPods to manage your dependencies, simply add ReactiveCocoa to your Podfile:

    pod 'ReactiveCocoa', '~> 5.0.0'

Git submodule
-------------

Add the ReactiveCocoa repository as a submodule of your application’s repository.

Run 

    git submodule update --init --recursive 

from within the ReactiveCocoa folder.

Drag and drop `ReactiveCocoa.xcodeproj`, `Carthage/Checkouts/ReactiveSwift/ReactiveSwift.xcodeproj`, and `Carthage/Checkouts/Result/Result.xcodeproj` into your application’s Xcode project or workspace.

On the “General” tab of your application target’s settings, add `ReactiveCocoa.framework`, `ReactiveSwift.framework`, and `Result.framework` to the “Embedded Binaries” section.

If your application target does not contain Swift code at all, you should also set the `EMBEDDED_CONTENT_CONTAINS_SWIFT` build setting to “Yes”.




