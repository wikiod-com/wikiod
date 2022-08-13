---
title: "Getting started with cocoapods"
slug: "getting-started-with-cocoapods"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Updating CocoaPods
To update CocoaPods by simply installing the gem again

    [sudo] gem install cocoapods

Or for a pre-release version

    [sudo] gem install cocoapods --pre

## Podfile sample
The dependencies for your projects are specified in a single text file called a Podfile. CocoaPods will resolve dependencies between libraries, fetch the resulting source code, then link it together in an Xcode workspace to build your project.
1. Create a podfile
    
       # Next line contains target platform settings
       platform :ios, '8.0'
       # Use dynamic Frameworks
       use_frameworks!
    
       # Target name
       target 'MyApp' do
         # List of target dependencies
         pod 'ObjectiveSugar', '~> 1.1'
         pod 'ORStackView', '~> 3.0'
         pod 'RxSwift', '~> 2.6'
       end

2. Install dependencies. The process of dependencies installation is done by executing this command through terminal in the project directory:

       pod install

3. Updating dependencies to new versions:
- Updating specific pod

      pod update RxSwift
- Updating all pods 

      pod update
[Using Cocoapods][1]


  [1]: https://guides.cocoapods.org/using/using-cocoapods.html

## Getting Started
Lets start installing the popular library [Alamofire][1] to our Xcode project!

Lets first install CocoaPods by using the command:
    
    [sudo] gem install cocoapods

Then let's create a new project in Xcode called Start! Navigate to the folder that contains the `.xcodeproj` and create a new text file called `podfile`!

Replace the `podfile` with the following: 

    source 'https://github.com/CocoaPods/Specs.git'
    platform :ios, '10.0'
    use_frameworks!
    
    target 'start' do
        pod 'Alamofire', '~> 4.3'
    end

Use the `cd` command to change to the directory containing the `.xcodeproj` and issue the command `pod install`. Alamofire is installed in the project 'start'!

Now, double-click the xcworkspace file (not .xcodeproj) and use Alamofire!


  [1]: https://github.com/Alamofire/Alamofire

