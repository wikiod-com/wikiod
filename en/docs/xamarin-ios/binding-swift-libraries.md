---
title: "Binding Swift Libraries"
slug: "binding-swift-libraries"
draft: false
images: []
weight: 9017
type: docs
toc: true
---

An easy to follow guide that will lead you through the process of binding Swift .framework files for use in a Xamarin project.

 1. When building a library in Xcode it has an option to include the swift libraries. Don’t! They will be included in your final app as NAME.app/Frameworks/LIBRARY.framework/Frameworks/libswift*.dylib but they must be included as NAME.app/Frameworks/libswift*.dylib

2. You can find this information elsewhere, but it’s worth mention: Don’t include Bitcode in the library. As of right now Xamarin don’t include Bitcode for iOS and Apple requires all libraries to support the same architectures.


## Binding a Swift Library in Xamarin.iOS
Binding a Swift Library in Xamarin.iOS follows the same process for Objective-C as shown in https://developer.xamarin.com/guides/ios/advanced_topics/binding_objective-c/, but with some caveats.

1.    A swift class must inherit from NSObject to be binded. 
2.    Swift compiler will translate class and protocol names into something else unless you use the [@objc][1] annotation (e.g. @objc(MyClass)) in your swift classes to specify the explicit objective c name.
3.    In runtime your APP must include some swift core libraries alongside your binded framework in a folder called Frameworks;
4.    When the App is pushed to AppStore it must include a SwiftSupport folder alongside your Payload folder. Those are inside the IPA file.

Here you can find a simple sample binding: https://github.com/Flash3001/Xamarin.BindingSwiftLibrarySample

And a full binding sample: https://github.com/Flash3001/iOSCharts.Xamarin

Please find steps below:
# 1.1 Prepare the Swift classes you want to export
For any Swift class you want to use you either have to inherit from NSObject and make the Objective-C name explicit using the objc annotation. Otherwise the Swift compiler will generate different names. Below is an example code of how a Swift class could look like. Note that it doesn't matter which class it inherits as long as the root class inherits from NSObject.

    //Add this to specify explicit objective c name
    @objc(MyClass)
    open class MyClass: NSObject {
        open func getValue() -> String
        {
            return "Value came from MyClass.swift!";
        }
    }

# 1.2 Build the framework

Disable Bitcode. *

[![Disable Bitcode][2]][2]

Build for release for Device and Simulator. *
[![Set it as Release in Xcode Schemes][3]][3]
[![Build for Device][4]][4]

[![Build for Simulator][5]][5]

[![Find frameworks][6]][6]

[![Frameworks][7]][7]

* Not related only to Swift binding. 


# 2. Create a fat library

A framework contains several files, the one that needs to eat a little is NAME.framework/NAME (without extension).

 - Copy Release-iphoneos/NAME.framework to NAME.framework 
 - Create the FAT library using:
     -    **lipo -create Release-iphonesimulator/NAME.framework/NAME 
Release-iphoneos/NAME.framework/NAME -output NAME.framework/NAME**
 - Copy the files in Release-iphonesimulator/NAME.framework/Modules/NAME.swiftmodule to NAME.framework/Modules/NAME.swiftmodule (until now it only contained files from the iphoneos)

[![Create fat library][8]][8]

[![Copy pending files][9]][9]

[![Files copied][10]][10]


 
  
# 3. Import the library

I’ll assume you already created the Binding project in File -> New -> iOS -> Binding Library.

Xamarin support importing .frameworks. Just right click ‘Native References’ and click in ‘Add native reference’. Find the newly created fat framework and add it. 

[![Add Native Reference][11]][11]

[![Select .framework][12]][12]

[![Reference][13]][13]


  
# 4. Create the ApiDefinition based on LIBRARY-Swift.h file inside headers.

You can do it manually, but it won’t be nice. You can use Objetive Sharpie. The tool Xamarin uses to bind its own libraries. 

How to use it on https://developer.xamarin.com/guides/cross-platform/macios/binding/objective-sharpie/

The basic command will be something like: ***sharpie bind -sdk iphoneos9.3 NAME-Swift.h***

If you get a `System.Reflection.TargetInvocationException` it is probably because you have a different SDK version installed. Run the following command to check with iPhone OS SDK you have installed:

    sharpie xcode -sdks

The file ***NAME-Swift.h*** is located in ***NAME.framework/Headers/NAME-Swift.h***

Note: The swift classes must inherit from "NSObject", otherwise ***NAME-Swift.h*** won't import your classes and Objetive Sharpie will convert nothing.

[![Sharpie][13]][13]

Replace the contents of your binding project ApiDefinition.cs with the newly created one.
[![Replace file contents][14]][14]

# 5. Change all [Protocol] and [BaseType] to include the class’ name in Objective-C runtime.

In case the original Swift class or protocol doesn't include the @objc(MyClass) annotation as specified in step 1.1 they will have its internal Objective-C names changed, so you need to map it to the right one.

All names are available in the file NAME-Swift.h in the following format:

    SWIFT_CLASS("_TtC11SwiftSample7MyClass")
    @interface MyClass : NSObject

And

    SWIFT_PROTOCOL("_TtP6Charts17ChartDataProvider_")
    @protocol ChartDataProvider

To set the name you use BaseTypeAttribute.Name https://developer.xamarin.com/guides/cross-platform/macios/binding/binding-types-reference/#BaseType.Name property for classes and ProcotolAttribute.Name https://developer.xamarin.com/api/property/MonoTouch.Foundation.ProtocolAttribute.Name/ for protocols.

    [BaseType(typeof(NSObject), Name = "_TtC11SwiftSample7MyClass")]
    interface MyClass

Doing it manually is not cool. You can use this tool https://github.com/Flash3001/SwiftClassify to insert all the names. (It’s work in progress. But it’s quite simple, just by looking at the code you will get how it works).


# 6.1 Include all Swift dependencies to run.

If you try to consume the library in an App and try to run it right now it will crash. The error is due to the lack of libswiftCore.dylib

Something like this:

    Dyld Error Message:
      Library not loaded: @rpath/libswiftCore.dylib
      Referenced from: /Users/USER/Library/Developer/CoreSimulator/Devices/AC440891-C819-4050-8CAB-CE15AB4B3830/data/Containers/Bundle/Application/27D2EC87-5042-4FA7-9B80-A24A8971FB48/SampleUsing.app/Frameworks/SwiftSample.framework/SwiftSample
      Reason: image not found

Xamarin.iOS doesn’t give official support for binding a Swift library. So you must manually include the swift core libraries in the Frameworks and SwiftSupport folders. The files for the Frameworks folder are different for Simulator and Device. They can be found in /Applications/Xcode.app/Contents/Developer//XcodeDefault.xctoolchain/usr/lib/swift.Toolchains

Instead of manually copying the files inside the Framework folder you can use this library https://github.com/Flash3001/Xamarin.Swift3.Support . It includes every single dependency Swift 3.1 needs, each one in a single NuGet package.

[![Add NuGet Package][16]][16]

As you can see, the Nuget Package is included in the consumer App, not in the binding itself. If you try to include it in the binding you will get compile errors. 

If you are building a Nuget Package you can instruct Nuget to include it as a dependency.

[![App Running][17]][17]

# 6.2. Finding out which Swift dependencies to include.

An important thing to do is to figure out each package you need to include in your project. A simple binding will usually need:

         libswiftCore.dylib
         libswiftCoreGraphics.dylib
         libswiftCoreImage.dylib
         libswiftDarwin.dylib
         libswiftDispatch.dylib
         libswiftFoundation.dylib
         libswiftObjectiveC.dylib
         libswiftQuartzCore.dylib
         libswiftUIKit.dylib   

To list each dependency you can run the following command inside your LibraryName.framework 

    otool -l -arch armv7 LibraryName | grep libswift

[![List dependencies][18]][18]

Do not include every package available in NuGet for Swift3 as they might grow your app size.

# 7. Include SwiftSupport to push App to AppStore.

Apple requires your App to be sent with a SwiftSupport folder alongside your Payload folder. Both are inside your IPA package.

You can use this script https://github.com/bq/ipa-packager to do this work for you.

This process is the only one the library consumer will have to do manually. Every time he/she tries to push the App to AppStore. 

[![Archive for Publishing][19]][19]

Click 'Sign and Distribute' and Save to Disk
[![Sign and Distribute][20]][20]

Unzip your .IPA
[![Unzip][21]][21]

Create the new IPA using the script before mentioned
[![New API][22]][22]

If you Unzip the file know, it will contain the SwiftSupport folder.
[![enter image description here][23]][23]


  
# Remarks

When building a library in Xcode it has an option to include the swift libraries. Don’t! They will be included in your final app as NAME.app/Frameworks/LIBRARY.framework/Frameworks/libswift*.dylib but they must be included as NAME.app/Frameworks/libswift*.dylib

You can find this information elsewhere, but it’s worth mention: Don’t include Bitcode in the library. As of right now Xamarin don’t include Bitcode for iOS and Apple requires all libraries to support the same architectures.


# Disclaimer
This guide is originally created by [Lucas Teixeira](http://stackoverflow.com/users/1698917/lucas-teixeira).
All credits belong to him. Thank you, Lucas.


  [1]: https://developer.apple.com/library/content/documentation/Swift/Conceptual/BuildingCocoaApps/InteractingWithObjective-CAPIs.html#//apple_ref/doc/uid/TP40014216-CH4-ID35
  [2]: http://i.stack.imgur.com/LStuW.png
  [3]: http://i.stack.imgur.com/0kbTg.png
  [4]: http://i.stack.imgur.com/d9doz.png
  [5]: http://i.stack.imgur.com/8bW84.png
  [6]: http://i.stack.imgur.com/MXUnB.png
  [7]: http://i.stack.imgur.com/OZotY.png
  [8]: http://i.stack.imgur.com/0XE0b.png
  [9]: http://i.stack.imgur.com/zmmCf.png
  [10]: http://i.stack.imgur.com/BYtfy.png
  [11]: http://i.stack.imgur.com/DVoC8.png
  [12]: http://i.stack.imgur.com/y0qai.png
  [13]: http://i.stack.imgur.com/DCveu.png
  [14]: http://i.stack.imgur.com/Sq9TQ.png
  [15]: http://i.stack.imgur.com/yh81i.png
  [16]: https://i.stack.imgur.com/Hye5k.png
  [17]: http://i.stack.imgur.com/Fx0GW.png
  [18]: https://i.stack.imgur.com/dlZKB.png
  [19]: http://i.stack.imgur.com/1MkHy.png
  [20]: http://i.stack.imgur.com/azNYM.png
  [21]: http://i.stack.imgur.com/IVSGg.png
  [22]: http://i.stack.imgur.com/v4ckT.png
  [23]: http://i.stack.imgur.com/7qT8K.png

